;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of XWINDOWS)

;;;
;;; File: XTERM
;;; Package: XWINDOWS
;;;
;;; Defines the style XTERM for Parsing and Printing.
;;; Written by DAN 2-28-88
;;; Allows us to run TPS3 in an XTERM (X10.4) window, as long as the normal
;;; font is vtsingle and the bold font is vtsymbold. 

(part-of XWINDOWS)

(deffile xterm
  (part-of xwindows)
  (extension lsp)
  (mhelp "Defines XTERM style printing and parsing."))

(context xwindows)

(eval-when (load compile eval)
(defstyle xterm
  (print-symbol pp-symbol-xterm)
  (print-space-p pp-space-p-xterm)
  (terpri-heuristics terpri-heuristics-generic)
  (print-typesym pp-typesym-xterm)
  (print-type-char pprinc)
  (print-indent indent-generic)
  (display-prefix terpri)
  (display-postfix terpri)
  (text-prefix noop)
  (begin-environment noop-1)
  (end-environment noop-1)
  (print-indent indent-generic)
  (print-line print-line-generic)
  (print-tab tab-generic)
  (print-nextpar nextpar-generic)
  (margin-correct margin-generic-tps)
  (text-postfix noop)
  (mhelp "XTERM stands for a terminal running xterm with normal font 
vtsingle and bold font vtsymbold.")))

(defflag xterm-ansi-bold
  (flagtype integer+)
  (default 53)
  (subjects system)
  (mhelp "The number corresponding to the ANSI code for switching to bold font.
The default is 53 (ASCII for character 5) which corresponds to blink (often displayed as bold).
An alternative is 49 (ASCII for character 1) which is the ANSI standard for bold.

Further information is contained in the User's Manual and Programmer's Guide."))

(eval-when (load eval compile)
(defvar xterm-characters nil)
(defvar xterm-bold-in-use nil)
(defvar xterm-end-bold '(((27) (91)(48)(109))))
)

(defun pp-symbol-xterm (symbol)
  (when (and (not ppwfflag) curpos RIGHTMARGIN (> curpos RIGHTMARGIN)) ; cebrown 4/26/02
    (terpri) ; cebrown 4/26/02
    (setq curpos 0)) ; cebrown 4/26/02
  (cond ((stringp symbol) 
	 (when (and (not ppwfflag) curpos) (setq curpos (+ curpos (length symbol)))) ; cebrown 4/26/02
	 (pprinc symbol))
	((eq symbol 'not)
	 (when (and (not ppwfflag) curpos) (incf curpos))
	 (pprinc "~"))
	(t (let ((x-sym (cdr (assoc symbol xterm-characters))))
	     (cond (x-sym 
		    (pp-xterm-special-symbol symbol x-sym))
		   ((get symbol 'face)
		    (mapc #'pp-xterm-special-symbol (get symbol 'face)))
		   (t
		    (when (and (not ppwfflag) curpos) (setq curpos (+ curpos (length (princ-to-string symbol))))) ; cebrown 4/26/02
		    (pprinc symbol)))))))


(defun pp-xterm-special-symbol
  (symbol &optional (x-sym (cdr (assoc symbol xterm-characters))))
  (if x-sym
      (progn
	(when (and (not ppwfflag) curpos) (incf curpos)) ; cebrown 4/26/02
	(pptyox x-sym))
    (progn
      (when (and (not ppwfflag) curpos) (setq curpos (+ curpos (length (princ-to-string symbol))))) ; cebrown 4/26/02
      (pprinc symbol))))


(defun pp-space-p-xterm (symbol pc)
  (cond ((member pc '(" " "." "[" "~") :test #'string=) nil)
	((string-equal pc "NOT") nil)
	((not (symbolp pc)) t)
	((get pc 'infix) t)
	((memq pc '(setintersect setunion sigma1)) nil)
	((binder-q pc) (and (not (cdr (assoc pc xterm-characters)))
			    (not (get pc 'face))))
	((string= symbol "[") nil)
	((member symbol '("." " " "]") :test #'string=) nil)
	((get symbol 'infix) t)
	((binder-q symbol) nil)
	(first-order-print-mode nil)
	(t t)))


(defun pp-typesym-xterm (tp)
  (cond ;((null ppvirtflag) (pp-typesym-generic tp))
	((null tp)
	 ;;An illegal type symbol was somehow created, eg during
	 ;;parsing - don't print anything.
	 )
	((atom tp) 
	 (print-greek-type-char tp))
        (t (pp-typesym-xterm (car tp))
           (cond ((consp (cdr tp))
		  (pptyox 40) ;; print a left subparen
		  (pp-typesym-xterm (cdr tp))
		  (pptyox 41)) ;; print a right subparen
		 (t (pp-typesym-xterm (cdr tp)))))))


(defun print-greek-type-char (tp)
  (let ((num (char-int (character (symbol-name tp)))))
    (if (and (< num 91) 
	     (> num 64)) ; tp is an uppercase alphabetic char
	(pptyox (+ num 32)) ; print its lower case in bold
      (pprinc tp))))


;;; Following two used only if not pretty-printing
;;; In xterm, switch to bold font by using <ESC> [ 5 m
;;; which is Ascii 27 91 53 109  ; actually, this is the ANSI standard for blink, use 27 91 xterm-ansi-bold 109 instead -- ceb Jan 28 2005
;;; Switch back to normal by using <ESC> [ 0 m, or
;;; Ascii 27 91 48 109

(defun xterm-bold-font ()
  (unless xterm-bold-in-use 
	  (dont-count
	    (dolist (x (list 27 91 xterm-ansi-bold 109)) (tyo x)))
	  (setq xterm-bold-in-use t)))

(defun xterm-normal-font ()
  (when xterm-bold-in-use 
	(dont-count
	  (dolist (x '(27 91 48 109)) (tyo x)))
	  (setq xterm-bold-in-use nil)))



(defun pptyox (n)
  (if ppvirtflag
      (progn (incf ppwfflength)
	     (setq ppwfflist
		   (nconc ppwfflist
			  (list (list (list '(27) '(91) (list xterm-ansi-bold) '(109)))
				(list n) xterm-end-bold))))
    (unwind-protect
	(progn (xterm-bold-font) (tyo n))
      (xterm-normal-font))))



;;; These numbers refer to the corresponding characters from the X bold fonts 
;;; vtsymbold and galsymbold.

(setq xterm-characters
  '((lambda . 38)
    (equiv . 61)
    (neg . 29)
    (forall . 59)
    (exists . 39)
    (implies . 45)
    (and . 91)
    (or . 92)
    (assert . 123)
    (leftsemanticbracket . 32)
    (ceiling1 . 33)
    (ceiling2 . 34)
    (floor1 . 35)
    (floor2 .  36)
    (bigbar . 37)
    (sublparen . 40)
    (subrparen . 41)
    (truth  . 42)
    (falsehood . 43)
    (intersect . 44)
    (propersuperset . 45)
    (union . 46)
    (subset . 47)
    (sup0 . 48)
    (sup1 . 49)
    (sup2 . 50)
    (sup3 . 51)
    (sup4 . 52)
    (sup5 . 53)
    (sups . 53) ; a hack which doesn't look too bad used for EQUIVS
    (sup6 . 54)
    (sup7 . 55)
    (sup8 . 56)
    (sup9 . 57)
    (propersubset . 58)
    (setintersect . 60)
    (setunion . 62)
    (capsigma . 63)
    (sub1 . 64)
    (scripta . 65)
    (scriptb . 66)
    (scriptc . 67)
    (scriptd . 68)
    (scripte . 69)
    (scriptf . 70)
    (scriptg . 71)
    (scripth . 72)
    (scripti . 73)
    (scriptj . 74)
    (scriptk . 75)
    (scriptl . 76)
    (scriptm . 77)
    (scriptn . 78)
    (scripto . 79)
    (scriptp . 80)
    (powerset . 80)
    (scriptq . 81)
    (scriptr . 82)
    (scripts . 83)
    (scriptt . 84)
    (scriptu . 85)
    (scriptv . 86)
    (scriptw . 87)
    (scriptx . 88)
    (scripty . 89)
    (scriptz . 90)
    (nat . 93)
    (eqp . 94)
    (iota . 95)
    (rightsemanticbracket . 96)
    (subalpha . 97)
    (subbeta . 98)
    (subxi . 99)
    (subdelta . 100)
    (subepsilon . 101)
    (subphi . 102)
    (subgamma . 103)
    (subeta . 104)
    (subiota . 105)
    (subnullset . 106)
    (subkappa . 107)
    (sublambda . 108)
    (submu . 109)
    (subnu . 110)
    (subomicron . 111)
    (subpi . 112)
    (subtheta . 113)
    (subrho . 114)
    (subsigma . 115)
    (subtau . 116)
    (subupsilon . 117)
    (submember . 118)
    (subomega  . 119)
    (subchi . 120)
    (subpsi . 121)
    (subzeta . 122)
    (one . 124)
    (valid . 125)
    (nullset . 126)))

(eval-when (load compile eval)
(defflag window-style
  (flagtype dev-style)
  (default xterm)
  (irrelevancy-preconditions
   (use-window-style (not use-window-style)))
  (pre-change-fn (lambda (flag value old-value) ; cebrown 3/30/02
		   (declare (special *using-interface*) (ignore flag old-value))
		   (when *using-interface*
		     (unless (equal value 'istyle)
		       (throwfail "Must Use ISTYLE When Running TPS Using An Interface")))))
  (change-fn (lambda (a b c)
	       (declare (special *using-interface*))
	       (unless *using-interface* 
		 (when (eq b 'istyle)
		   (msgf "Cannot Use ISTYLE Unless Running TPS Using An Interface")
		   (set a c)))
	       (when (eq b 'concept-s) (reset))))
  (subjects window-props)
  (mhelp "The style of output that will be used in all the windows
besides the main one, if USE-WINDOW-STYLE is T. Ignored if
USE-WINDOW-STYLE is NIL.")))

(defflag use-window-style
  (flagtype boolean)
  (default T)
  (subjects window-props)
  (mhelp "If T, uses the style given by WINDOW-STYLE for output
to windows other than the main one. If NIL, windows will all be
in the style given by STYLE."))
