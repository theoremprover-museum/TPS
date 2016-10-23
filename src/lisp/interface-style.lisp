;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of wff-print)

;;;
;;; File: ISTYLE
;;; Package: CORE
;;;
;;; Defines the style ISTYLE for Printing.
;;; Written by CEB 3/24/02
;;; Allows us to run TPS3 with an interface.

(deffile interface-style
  (part-of wff-print)
  (extension lsp)
  (mhelp "Defines ISTYLE style printing and parsing."))

(context wff-printing)

(eval-when (load compile eval)
(defstyle istyle
  (print-symbol pp-symbol-istyle)
  (print-space-p pp-space-p-istyle)
  (terpri-heuristics terpri-heuristics-generic)
  (print-typesym pp-typesym-istyle)
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
  (mhelp "ISTYLE stands for tps running with an interface."))
)

(eval-when (load eval compile)
(defvar istyle-characters nil)
(defvar istyle-sym-in-use nil)
(defvar istyle-start-sym '(((0) (1))))
(defvar istyle-end-sym '(((0) (0))))
)

(defun pp-symbol-istyle (symbol)
  (cond ((stringp symbol) 
	 (pprinc symbol))
	((eq symbol 'not) (pprinc "~"))
	(t (let ((x-sym (cdr (assoc symbol istyle-characters))))
	     (cond (x-sym 
		     (pp-istyle-special-symbol symbol x-sym))
		   ((get symbol 'face)
		    (mapc #'pp-istyle-special-symbol (get symbol 'face)))
		   (t (pprinc symbol)))))))


(defun pp-istyle-special-symbol
  (symbol &optional (x-sym (cdr (assoc symbol istyle-characters))))
  (if x-sym
      (pptyoisym x-sym)
    (pprinc symbol)))


(defun pp-space-p-istyle (symbol pc)
  (cond ((member pc '(" " "." "[" "~") :test #'string=) nil)
	((string-equal pc "NOT") nil)
	((not (symbolp pc)) t)
	((get pc 'infix) t)
	((memq pc '(setintersect setunion sigma1)) nil)
	((binder-q pc) (and (not (cdr (assoc pc istyle-characters)))
			    (not (get pc 'face))))
	((string= symbol "[") nil)
	((member symbol '("." " " "]") :test #'string=) nil)
	((get symbol 'infix) t)
	((binder-q symbol) nil)
	(first-order-print-mode nil)
	(t t)))


(defun pp-typesym-istyle (tp)
  (cond ;((null ppvirtflag) (pp-typesym-generic tp))
	((null tp)
	 ;;An illegal type symbol was somehow created, eg during
	 ;;parsing - don't print anything.
	 )
	((atom tp) 
	 (print-greek-type-char-istyle tp))
        (t (pp-typesym-istyle (car tp))
           (cond ((consp (cdr tp))
		  (pptyoisym (list 1 40)) ;; print a left subparen
		  (pp-typesym-istyle (cdr tp))
		  (pptyoisym (list 1 41))) ;; print a right subparen
		 (t (pp-typesym-istyle (cdr tp)))))))

(defun print-greek-type-char-istyle (tp)
  (let ((num (char-int (character (symbol-name tp)))))
    (if (and (< num 91) 
	     (> num 64)) ; tp is an uppercase alphabetic char
	(pptyoisym (list 1 (+ num 32))) ; print its lower case in bold
      (pprinc tp))))

;;; Following two used only if not pretty-printing
;;; In istyle, switch to sym font by using 0 1
;;; Switch back to normal by using 0 0
(defun istyle-sym-font ()
  (unless istyle-sym-in-use 
	  (dont-count
	    (dolist (x '(0 1)) (tyo x)))
	  (setq istyle-sym-in-use t)))

(defun istyle-normal-font ()
  (when istyle-sym-in-use 
	(dont-count
	  (dolist (x '(0 0)) (tyo x)))
	  (setq istyle-sym-in-use nil)))

(defun pptyoisym (n)
  (if ppvirtflag
      (progn (incf ppwfflength)
	     (setq ppwfflist
		   (nconc ppwfflist
			  (append (list istyle-start-sym)
				  (mapcar #'list n)
				  (list istyle-end-sym)))))
    (unwind-protect
	(progn
	  (istyle-sym-font) 
	  (dolist (b n)
	    (tyo b)))
      (istyle-normal-font))))

(defun short-to-bytes (n)
  (let* ((lsb (mod n 256))
	 (msb (mod (/ (- n lsb) 256) 256)))
    (list msb lsb)))

(defun start-prompt-msg ()
  (tyo 0)
  (tyo 2))

(defun start-prompt-name ()
  (tyo 0)
  (tyo 3))

(defun start-prompt-argtyp ()
  (tyo 0)
  (tyo 4))

(defun start-prompt-options ()
  (tyo 0)
  (tyo 5))

(defun start-prompt-default ()
  (tyo 0)
  (tyo 6))

(defun start-prompt-help ()
  (tyo 0)
  (tyo 7))

(defun end-prompt ()
  (tyo 0)
  (tyo 8)
  (force-output))

(defun command-finished ()
  (tyo 0)
  (tyo 9)
  (force-output))

(defun send-top-level ()
  (tyo 0)
  (tyo 10)
  (msg top-level)
  (tyo 0)
  (tyo 10)
  (force-output))

#-(and allegro-version>= (version>= 5 0))
(defun open-window-with-socket (name title width height &optional (big nil))
  (declare (ignore name title width height big))
  (throwfail "Sorry - can only open XTerm style Windows for the Interface when Running under Allegro >= 5.0"))

#+(and allegro-version>= (version>= 5 0))
(defun open-window-with-socket (name title width height &optional (big nil))
  (let* ((pass (acl-socket:make-socket :connect :passive))
	 (local (acl-socket:local-port pass)))
    (tyo 0)
    (tyo 11)
    (msg local)
    (tyo 0)
    (tyo 12)
    (msg title)
    (tyo 0)
    (tyo 13)
    (msg width)
    (tyo 0)
    (tyo 14)
    (msg height)
    (tyo 0)
    (if big
	(tyo 16)
      (tyo 15))
    (force-output)
    (acl-socket:accept-connection pass)))

(defun clear-window (sock)
  (when (and (streamp sock) (open-stream-p sock))
    (let ((*standard-output* sock))
      (tyo 0)
      (tyo 17)
      (force-output))))

(defun change-color (color)
  (case color
	(red (tyo 0) (tyo 19) (tyo 1))
	(blue (tyo 0) (tyo 19) (tyo 2))
	(green (tyo 0) (tyo 19) (tyo 3))
	(t (tyo 0) (tyo 19) (tyo 0))) ; black
  (force-output))

(defun close-window (sock)
  (when (and (streamp sock) (open-stream-p sock))
    (let ((*standard-output* sock))
      (tyo 0)
      (tyo 18)
      (force-output)
      (close sock))))

;;; These numbers are sent to an interface to name the corresponding symbol.
;;; Each symbol should be associated with two numbers, each between 1 and 127

(setq istyle-characters
  '((lambda 1 38)
    (equiv 1 61)
    (neg 1 29)
    (forall 1 59)
    (exists 1 39)
    (implies 1 45)
    (and 1 91)
    (or 1 92)
    (assert 1 123)
    (leftsemanticbracket 1 32)
    (ceiling1 1 33)
    (ceiling2 1 34)
    (floor1 1 35)
    (floor2 1  36)
    (bigbar 1 37)
    (sublparen 1 40)
    (subrparen 1 41)
    (truth  1 42)
    (falsehood 1 43)
    (intersect 1 44)
    (propersuperset 1 45)
    (union 1 46)
    (subset 1 47)
    (sup0 1 48)
    (sup1 1 49)
    (sup2 1 50)
    (sup3 1 51)
    (sup4 1 52)
    (sup5 1 53)
    (sups 1 53) ; a hack which doesn't look too bad used for EQUIVS
    (sup6 1 54)
    (sup7 1 55)
    (sup8 1 56)
    (sup9 1 57)
    (propersubset 1 58)
    (setintersect 1 60)
    (setunion 1 62)
    (capsigma 1 63)
    (sub1 1 64)
    (scripta 1 65)
    (scriptb 1 66)
    (scriptc 1 67)
    (scriptd 1 68)
    (scripte 1 69)
    (scriptf 1 70)
    (scriptg 1 71)
    (scripth 1 72)
    (scripti 1 73)
    (scriptj 1 74)
    (scriptk 1 75)
    (scriptl 1 76)
    (scriptm 1 77)
    (scriptn 1 78)
    (scripto 1 79)
    (scriptp 1 80)
    (powerset 1 80)
    (scriptq 1 81)
    (scriptr 1 82)
    (scripts 1 83)
    (scriptt 1 84)
    (scriptu 1 85)
    (scriptv 1 86)
    (scriptw 1 87)
    (scriptx 1 88)
    (scripty 1 89)
    (scriptz 1 90)
    (nat 1 93)
    (eqp 1 94)
    (iota 1 95)
    (rightsemanticbracket 1 96)
    (subalpha 1 97)
    (subbeta 1 98)
    (subxi 1 99)
    (subdelta 1 100)
    (subepsilon 1 101)
    (subphi 1 102)
    (subgamma 1 103)
    (subeta 1 104)
    (subiota 1 105)
    (subnullset 1 106)
    (subkappa 1 107)
    (sublambda 1 108)
    (submu 1 109)
    (subnu 1 110)
    (subomicron 1 111)
    (subpi 1 112)
    (subtheta 1 113)
    (subrho 1 114)
    (subsigma 1 115)
    (subtau 1 116)
    (subupsilon 1 117)
    (submember 1 118)
    (subomega  1 119)
    (subchi 1 120)
    (subpsi 1 121)
    (subzeta 1 122)
    (one 1 124)
    (valid 1 125)
    (nullset 1 126)
    (epsilon 2 1)))

