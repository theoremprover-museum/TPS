;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of OTLSCRIBE)

;;;
;;; File: OTL-SCRIBEOUT

(part-of otlscribe)

(deffile otl-scribeout
  (part-of otlscribe)
  (extension lsp)
  (mhelp "Contains functions which allow writing into files inside the
outline package."))

(context otl-files)

(defflag scribe-preamble
  (flagtype string)
  (default "")
  (subjects printing)
  (mhelp "The preamble that is printed into the first lines of all 
the Scribe files produced by TPS, except those that are in SLIDES
style. See also SLIDES-PREAMBLE, TEX-PREAMBLE."))

(defflag scribe-postamble
  (flagtype string)
  (default ""); having @End(Verbatim) messes up the facilities file. MB Mon Dec  5 14:52:49 1994
  (subjects printing)
  (mhelp "The postamble that is printed into all Scribe files
immediately before they are closed by TPS. See SCRIBE-PREAMBLE."))
 
(defflag scribe-line-width
  (flagtype integer+)
  (default 75)
  (subjects otl-vars)
  (mhelp "Width of a proofline in characters."))

;the next five things were variables, but aren't any more.

(defflag tpstex 
  (flagtype string)
  (subjects printing-tex)
  (default "")
  (mhelp "The pathname of the tps.tex file on your system. Should 
be initialized by the tps3.ini file."))

(defflag vpdtex 
  (flagtype string)
  (subjects printing-tex)
  (default "")
  (mhelp "The pathname of the vpd.tex file on your system. Should 
be initialized by the tps3.ini file."))

(defflag tex-preamble 
  (flagtype string)
  (subjects printing-tex)
  (default "")
  (mhelp "The preamble that is printed into the beginning of all TeX 
files produced by TPS. See also VPFORM-TEX-PREAMBLE, TEX-1-PREAMBLE, 
TEX-POSTAMBLE."))

; this used to be the value of tex-preamble, but it's really specific to certain flag settings,
; so I separated it out. - cebrown 3/10/03
(setq *tex-halign-preamble*
      "\\raggedright
{\\baselineskip=4pt
\\halign{#\\hfil&\\quad\\vtop{\\rightskip=0truept plus 1truein\\parindent=0truept\\hsize=1truein#}\\hfil&\\quad#
          \\hfil&\\quad\\vtop{\\rightskip=0truept plus 3truein\\parindent=0truept\\hsize=3.5truein\\tabskip=0.5truein plus 1truein minus 0.45truein#}
          \\hfil&\\quad\\vtop{\\rightskip=0truept plus 1truein\\parindent=0truept\\hsize=1truein#}\\hfil\\cr
")

(defflag latex-preamble 
  (flagtype string)
  (subjects printing-tex)
  (default "\\documentclass{article}
\\setlength{\\parindent}{0pt}
\\topmargin 0in
\\footskip 0pt
\\textheight 8.5in
\\oddsidemargin 0in
\\evensidemargin 0pt
\\textwidth 7in 
\\def\\endf{\\end{document}}
\\input /afs/andrew/mcs/math/TPS/doc/lib/tps.sty
\\input /afs/andrew/mcs/math/TPS/doc/lib/tps.tex
\\input /afs/andrew/mcs/math/TPS/doc/lib/vpd.tex
\\newcommand{\\markhack}[1]{\\vspace*{-0.6in}{#1}\\vspace*{0.35in}\\markright{{#1}}}
%a hack to get us a fake header on page 1 without having to do begin{titlepage} ~ end{titlepage}
\\begin{document}
")
  (mhelp "The preamble that is printed into the beginning of all TeX 
files produced by TPS when LATEX-EMULATION is T."))

(defflag tex-1-preamble 
  (flagtype string)
  (subjects printing-tex)
  (default "\\parindent=0pt
")
  (mhelp "Another TeX preamble, used when TEX-MIMIC-SCRIBE is T. 
See TEX-PREAMBLE."))

(defflag tex-postamble 
  (flagtype string)
  (subjects printing-tex)
  (default "\\eject\\end")
  (mhelp "The standard way in which TPS will end a TeX file.
See TEX-PREAMBLE, TEX-1-POSTAMBLE."))

(defflag tex-1-postamble 
  (flagtype string)
  (subjects printing-tex)
  (default "\\vfill\\eject\\end")
  (mhelp "Another TeX postamble, used when TEX-MIMIC-SCRIBE is T.
See TEX-POSTAMBLE."))

(defflag latex-postamble 
  (flagtype string)
  (subjects printing-tex)
  (default "\\end{document}")
  (mhelp "The standard way in which TPS will end a TeX file
when LATEX-EMULATION is T."))

(defflag tex-line-width
  (flagtype integer+)
  (default 75)
  (subjects otl-vars)
  (mhelp "width of a proofline in characters."))

(defmexpr scribeproof
  (argtypes filespec yesno)
  (argnames filename timing)
  (arghelp "Filename" "Recording time?")
  (defaultfns (lambda (filename timing)
		(list
		  (if (eq filename '$)
		      (namestring
			(make-pathname%
			  :name (string-downcase (symbol-name dproof))
			  :type "mss"))
		    filename)
		  (if (eq timing '$) nil timing))))
  (print-command t)
  (mhelp "Print the current proof into a MSS file.
After leaving TPS, run this .MSS file through Scribe and print the resulting
file."))

(defun scribeproof (filename timing)
  (reroute-output filename
		  (namestring (make-pathname%
				:name (string-downcase (symbol-name dproof))
				:type "mss"))
		  (if use-internal-print-mode
		      (in-mode scribe-otl
			       (msgf scribe-preamble)
			       (format T "~%@PageHeading(Immediate, Left <~A>~
				     ,Center <@value(Page)>, Right <~A>)~
				     ~%@PageFooting(Immediate, Right <~A>)~
				     ~%@Begin(Verbatim, LineWidth ~D)~%"
				       dproof (status-userid) (stringdt nil)
				       scribe-line-width)
			       (pall)
			       (if (and timing (auto::whether-called 'auto::mating))
				   (auto::display-time 'all))
			       (when (and print-comments (get dproof 'comment)) (msg t t t "@i(") (eval (get dproof 'comment)) (msg ")" t))
			       (msgf scribe-postamble))
		    (let ((style 'scribe))
		      (msgf scribe-preamble)
		      (format T "~%@PageHeading(Immediate, Left <~A>~
				     ,Center <@value(Page)>, Right <~A>)~
				     ~%@PageFooting(Immediate, Right <~A>)~
				     ~%@Begin(Verbatim, LineWidth ~D)~%"
			      dproof (status-userid) (stringdt nil)
			      scribe-line-width)
		      (pall)
		      (when (and print-comments (get dproof 'comment)) (msg t t t "@i(") (eval (get dproof 'comment)) (msg ")" t))
		      (msgf scribe-postamble)))
		  ))

(defmexpr texproof
  (argtypes filespec yesno)
  (argnames filename timing)
  (arghelp "filename" "Recording time?")
  (defaultfns (lambda (filename timing)
                (list
                  (if (eq filename '$)
                      (namestring
                        (make-pathname%
                          :name (string-downcase (symbol-name dproof))
                          :type "tex"))
                    filename)
                  (if (eq timing '$) nil timing))))
  (print-command t)
  (mhelp "Print the current proof into a tex file.
After leaving tps, run this .tex file through tex and print the resulting
file.

Many flags affect the output of texproof.
See: USE-INTERNAL-PRINT-MODE, TURNSTILE-INDENT-AUTO, TURNSTILE-INDENT,
LATEX-EMULATION, TEX-MIMIC-SCRIBE, PPWFFLAG, DISPLAYWFF, INFIX-NOTATION,
PAGELENGTH, PAGEWIDTH, TEX-BREAK-BEFORE-SYMBOLS, LOCALLEFTFLAG, SCOPE,
ALLSCOPEFLAG, USE-DOT, FIRST-ORDER-PRINT-MODE, FILLINEFLAG, ATOMVALFLAG."))

(defun texproof (filename timing)
  (texproof-generic filename timing dproof))

(defun texproof-generic (filename timing prfname)
  (unless (and *running-remotely* (not *expert-running-remotely*))
    (if (not (probe-file tpstex))
	(progn (princ "Please input the correct full pathname of the file tps.tex.")
	       (terpri)
	       (setq tpstex (read-line)))))
  (if (not (probe-file tpstex)) (throwfail "The file tps.tex does not exist."))
  (reroute-output filename
                  (namestring (make-pathname%
                                :name (string-downcase (symbol-name prfname))
                                :type "tex"))
                  (progn
		    (if latex-emulation
			(msgf latex-preamble t "\\markhack{{\\bf " prfname "}\\hfill " 
			      (stringdt nil) "\\hfill{\\bf " (status-userid) "}}")
		      (progn (princ "\\input ") (princ tpstex) (terpri)
			     (princ "\\headline={\\noindent{\\bf ") (princ prfname)
			     (princ "}\\hfil\\folio\\hfil{\\bf ")
			     (princ (status-userid))
			     (princ "}}") (princ "\\vskip36pt ")
			     (princ "\\footline={\\hfil ") (princ (stringdt nil)) (princ "}")
			     (if tex-mimic-scribe
				 (msgf tex-1-preamble)
			       (progn
				 (msgf tex-preamble)
				 (when (or ppwfflag displaywff)
				   (msgf *tex-halign-preamble*))))))
		    (if use-internal-print-mode
			(if tex-mimic-scribe
			    (in-mode tex-1-otl
				     (pall)
				     (if (and timing (auto::whether-called 'auto::mating))
					 (auto::display-time 'all))
				     (when (and print-comments (get dproof 'comment)) (msg t "{\\it ") (eval (tex-fudge-comment (get dproof 'comment))) (msg "}" t))
				     (if latex-emulation (msgf latex-postamble) (msgf tex-1-postamble)))
			  (in-mode tex-otl
				   (pall)
				   (unless latex-emulation
				     (when (or ppwfflag displaywff)
				       (princ "}}"))) ; closing the \halign here BEFORE the comments - cebrown 4/25/02
				   (when (and print-comments (get dproof 'comment)) (msg t "{\\it ") (eval (tex-fudge-comment (get dproof 'comment))) (msg "} " t))
				   (when (and timing (auto::whether-called 'auto::mating))
				     (auto::display-time 'all))
				   (princ "\\vfill ")
				   (if latex-emulation (msgf latex-postamble) (msgf tex-postamble))))
		      (let ((style (if tex-mimic-scribe 'tex-1 'tex))
			    (RIGHTMARGIN PAGEWIDTH))
			(declare (special style rightmargin))
			(pall)
			(unless (or latex-emulation tex-mimic-scribe (and (not ppwfflag) (not displaywff)))
			  (msg "\\cr " t) ; added carraige return - 4/24/2002
			  (princ "}}"))
			(when (and print-comments (get dproof 'comment))
			  (msg t "{\\it ") (eval (tex-fudge-comment (get dproof 'comment))) (msg "}" t))
			(when (and timing (auto::whether-called 'auto::mating))
			  (auto::display-time 'all))
			(princ "\\vfill")
			(if latex-emulation
			    (msgf latex-postamble)
			  (if tex-mimic-scribe
			      (msgf tex-1-postamble)
			    (msgf tex-postamble)))))
		    )))


;;; the following function PRINT-LINE-SCRIBE is the PRINT-LINE
;;; attribute of device style SCRIBE.


(defun print-line-scribe (line)
  (let ((hatomalist nil) (curpos 0) justificationlength)
    (declare (special hatomalist))
    (let ((line-num (format nil "(~S)" (linealias line))))
      (setq curpos (length line-num))
      (princ line-num))
    (if (and (neq turnstile-indent-auto 'compress) (> turnstile-indent 6)) (indentation 6) (msg " "))
    #+comment(let ((hyp (format nil "~{~S,~}"
		       (mapcar #'linealias (get line 'hypotheses)))))
      (setq curpos (+ curpos (max 0 (1- (length hyp)))))
      ;;(setq curpos (+ 5 (length hyp)))
      (princ (string-right-trim '(#\,) hyp)))
    (do* ((hyplist (mapcar #'linealias (get line 'hypotheses)) (cdr hyplist))
	  (hyp (princ-to-string (car hyplist)) (princ-to-string (car hyplist))))
	 ((string= "NIL" hyp) (if (> (+ 5 curpos) rightmargin) (progn (terpri) (setq curpos 0))))
	 (if (> (+ curpos (+ 2 (length hyp))) rightmargin) 
	     (progn (terpri) (setq curpos 0) (indentation 6)))
	 (setq curpos (+ curpos (length hyp))) (princ hyp) 
	 (unless (null (cdr hyplist)) (setq curpos (1+ curpos)) (princ ",")))
    (if (eq turnstile-indent-auto 'fix) (indentation turnstile-indent)  ;;13
      (if (eq turnstile-indent-auto 'min) (if (> (+ 5 (figure-out-indent)) rightmargin)
					      (indentation turnstile-indent)
					    (indentation (figure-out-indent)))
	(msg " ")))
    ;;(msg (t 13))
    (pcall print-symbol '!)
    (setq curpos (1+ curpos))
    ;;(if (> curpos 14) (setq curpos (1+ curpos)))
    (cond ((not (fixp rightmargin))
	   (setq rightmargin (linewidth))))  ;;(linelength nil)
    (setq hatomalist 
	  (prtwff (get line 'assertion)
		  (leftmargin (+ 1 curpos))))
    (unless ppwfflag
	  (unless auto-doc-flag (terpri))
	  (msg "@>")
	  (princjustification (get line 'justification))
	  (setq curpos rightmargin)
	  (return-from print-line-scribe rightmargin))
    (setq justificationlength (justlength (get line 'justification)))
    (when (> (+ curpos justificationlength) rightmargin)
      (setq curpos 0)
      (terpri))
    (msg "@>")
    (princjustification (get line 'justification))
    (setq curpos rightmargin)
    (when (and print-comments (get line 'comment) (listp (get line 'comment))) ;if it's a string, it's ""
	  (msg t "   @i(")
	  (eval (get line 'comment))
	  (msg ")" t)
	  (setq curpos 0))
    ))

(defun figure-out-indent (&optional (proof dproof))
  (let ((lines (get proof 'lines))
	(counter 0))
    (dolist (line lines)
	    (setq counter (max counter (count-space-needed line))))
    (1+ counter)))

(defun count-space-needed (line)
  (let* ((line-num (format nil "(~S)" (linealias line)))
	(counter (if (> turnstile-indent 6) 6 (length line-num)))
	(hyp (format nil "~{~S,~}"
		     (mapcar #'linealias (get line 'hypotheses))))
	(counter (+ counter (max 0 (1- (length (string-right-trim '(#\,) hyp)))))))
	counter))

;;;******************************************************
(defun print-line-tex-1 (line)
  (if (and (not ppwfflag) (not displaywff))
      (print-line-tex-plain line)
    (let* ((hatomalist nil) (curpos 0) 
	   (turnstile-indent
	    (if (eq turnstile-indent-auto 'MIN)
		(1+ (figure-out-indent))
	      turnstile-indent)))
      (declare (special hatomalist turnstile-indent))
      (let ((line-num (format nil "(~S) " (linealias line))))
	(setq curpos (length line-num))
	(princ "\\vbox{\\indent") (terpri)
	(progn (princ line-num)))
      (pcall print-indent 6)
      (let ((hyp (format nil "~{~S,~}"
			 (mapcar #'linealias (get line 'hypotheses)))))
	(setq curpos (+ curpos (max 0 (1- (length hyp)))))
	(progn (princ (string-right-trim '(#\,) hyp)) (princ " ")) ; cebrown 2/27/03
	(when (member TURNSTILE-INDENT-AUTO '(MIN FIX)) ; cebrown 2/27/03
	  (pcall print-indent turnstile-indent)))
      (msg "\\turnstile") 
      (setq curpos (1+ curpos))
      (cond ((not (fixp rightmargin))
	     (setq rightmargin (linewidth))))  ;;(linelength nil)
      (setq hatomalist 
	    (prtwff (get line 'assertion)
		    (leftmargin (+ 2 curpos))))
      (unless ppwfflag
	(unless auto-doc-flag (terpri))
	(msg "\\hfill ") ; we put a space here 4/24/2002
	(let ((justlen (justlength (get line 'justification))))
	  (if (> (+ curpos justlen 10) rightmargin)
	      (progn
		(princ "\\ceblinejust{")
		(princjustification (get line 'justification))
		(princ "}"))
	    (progn
	      (princ "\\hfill{")
	      (princjustification (get line 'justification))
	      (princ "}"))))
	(if (or (eq style 'tex-1) latex-emulation)
	    (msg "}\\filbreak " t) ; 4/26/2002 - \filbreak instead of \cr
	  (msg "}\\cr " t)) ; changed the dollar to a brace 4/24/2002, also added \cr & newline
	(setq curpos rightmargin)
	(return-from print-line-tex-1 rightmargin))
      (progn ;(setq curpos 0) 
	     (unless displaywff ; cebrown 4/25/02
	       (princ "\\lastformula"))
	     (let ((justlen (justlength (get line 'justification))))
	       (if (> (+ curpos justlen 10) rightmargin)
		   (progn
		     (princ "\\ceblinejust{")
		     (princjustification (get line 'justification))
		     (princ "}"))
		 (progn
		   (princ "\\hfill{")
		   (princjustification (get line 'justification)) 
		   (princ "}")))))
      (if (or (eq style 'tex-1) latex-emulation)
	  (progn (terpri) (princ "}\\filbreak"))
	(progn (terpri) (princ "}\\cr"))) ; it seems tex style must use \cr instead of \filbreak inside \halign - cebrown - 4/25/02
      (setq curpos rightmargin)
      (when (and print-comments (get line 'comment) (listp (get line 'comment))) ;if it's a string, it's ""
	(msg "\\vbox{\\indent ~ ~ {\\it ")
	(eval (tex-fudge-comment (get line 'comment)))
	(if (or (eq style 'tex-1) latex-emulation)
	    (msg "}}\\filbreak")
	  (msg "}}\\cr")) ; it seems tex style must use \cr instead of \filbreak inside \halign - cebrown - 4/25/02
	(setq curpos 0))
      )))

; cebrown 3/8/03
(defun print-line-tex-plain (line)
  (let ((hatomalist nil)
	(turnstile-ind 
	 (case turnstile-indent-auto
	   (FIX (floor (* tex-space-unit turnstile-indent)))
	   (MIN (floor (* tex-space-unit (1+ (figure-out-indent))))))))
    (declare (special hatomalist))
    (if (member turnstile-indent-auto '(VARY COMPRESS))
	(princ "\\cebproofline{")
      (princ "\\cebprooflinea{"))
    (princ (linealias line))
    (princ "}")
    (let ((hyp (format nil "~{~S,~}"
		       (mapcar #'linealias (get line 'hypotheses)))))
      (progn (princ "{") 
	     (princ (string-right-trim '(#\,) hyp)) (princ "}")))
    (when turnstile-ind
      (princ "{") (princ turnstile-ind) (princ "pt}"))
    (princ "{")
    (setq hatomalist 
	  (prtwff (get line 'assertion)
		  (leftmargin 2)))
    (princ "}{")
    (princjustification (get line 'justification))
    (princ "}")
    ))

(defun tex-fudge-comment (l)
  (if (or (null l) (and (not (listp l)) (string= l ""))) nil
    (if (and (listp (car l)) (eq (cdar l) 'gwff)) 
	(if displaywff
	    (append (list (car l)) (tex-fudge-comment (cdr l)))
	  (if ppwfflag
	      (append (list (car l) "\\lastformula ~") (tex-fudge-comment (cdr l)))
	    (append (list (car l) " ~") (tex-fudge-comment (cdr l)))))
      (cons (car l) (tex-fudge-comment (cdr l))))))

; Obsolete since Aug 28, 1997. Matt changed style tex to use print-line-tex-1:
;;; the following function print-line-tex is the print-line
;;; attribute of device style tex. 

;****************************************************************************
(defun print-line-tex (line)
  (let ((hatomalist nil) (curpos 0) justificationlength)
    (declare (special hatomalist))
    (let ((line-num (format nil "(~s)" (linealias line))))
      (setq curpos (length line-num))
      (princ line-num))
    (msg "&")
    (if (> turnstile-indent 6) (indentation 6))
    (let ((hyp (format nil "~{~,,1,@S,~}"
		       (mapcar #'linealias (get line 'hypotheses)))))
      (setq curpos (+ curpos (max 0 (1- (length hyp)))))
      ;;(setq curpos (+ 5 (length hyp)))
      (princ (string-right-trim '(#\,) hyp)))
    (indentation turnstile-indent)  ;;13
    ;;(msg (t 13))
    (msg "&\\turnstile&") ;hx: tempary method for (pcall print-symbol '!)
    (setq curpos (1+ curpos))
    ;;(if (> curpos 14) (setq curpos (1+ curpos)))
    (cond ((not (fixp rightmargin))
	   (setq rightmargin (linewidth))))  ;;(linelength nil)
    (setq hatomalist 
	  (prtwff (get line 'assertion)
		  (leftmargin (+ 2 curpos))))
    (msg "&")
    (unless ppwfflag
	  (unless auto-doc-flag (terpri))
	  (princjustification (get line 'justification))
	  (setq curpos rightmargin)
	  (return-from print-line-tex rightmargin))
    (setq justificationlength (justlength (get line 'justification)))
    (when (> (+ curpos justificationlength) rightmargin)
      (setq curpos 0)
      (terpri))
    (princjustification (get line 'justification))
    (msg "\\cr\\noalign{\\vskip6pt} ")
    (setq curpos rightmargin)
    (when (and print-comments (get line 'comment) (listp (get line 'comment))) ;if it's a string, it's ""
	  (msg "\\vbox{\\indent ~ ~ {\\it ")
	  (eval (tex-fudge-comment (get line 'comment)))
	  (msg "}}\\filbreak")
	  (setq curpos 0))
    ))

;************************************************************************
(defmexpr slideproof
  (argtypes filespec)
  (argnames filename)
  (arghelp "Filename")
  (defaultfns (lambda (filename)
		(list
		  (if (eq filename '$)
		      (namestring
			(make-pathname%
			  :name (string-downcase (symbol-name dproof))
			  :type "mss"))
		    filename))))
  (print-command t)
  (mhelp "Print the current proof into a MSS file. Use this command to make
slides.  After leaving TPS, run this .MSS file through Scribe and print the 
resulting file."))


(defun slideproof (filename)
  (reroute-output filename
		  (namestring (make-pathname%
				:name (string-downcase (symbol-name dproof))
				:type "mss"))
		  (let ((flag ppwfflag))
		    (if use-internal-print-mode
			(in-mode scribe-otl
				 (let ((rightmargin slides-width)
				       (turnstile-indent slides-turnstile-indent)
				       (ppwfflag flag))
				   (princ slides-preamble)
				   (format T "~%@Begin(Verbatim)~%")
				   (pall)
				   (when (and print-comments (get dproof 'comment)) (msg t t t "@i(") (eval (get dproof 'comment)) (msg ")" t))
				   (msgf scribe-postamble)))
		      (let ((style 'scribe))
			(princ slides-preamble)
			(format T "~%@Begin(Verbatim)~%")
			(pall)
			(when (and print-comments (get dproof 'comment)) (msg t t t "@i(") (eval (get dproof 'comment)) (msg ")" t))
			(msgf scribe-postamble)))
)))


(defmexpr setup-slide-style
  (mhelp "Sets flags to produce slides in scribe style."))

(defun setup-slide-style ()
  (setq rightmargin 51
        style 'scribe))
