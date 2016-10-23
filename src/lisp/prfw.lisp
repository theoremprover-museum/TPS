;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         This code was written as part of the TPS project at         ;;;
;;;                     Carnegie-Mellon University.                     ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@K.CS.CMU.EDU)               ;;;
;;; ******************************************************************* ;;;

(in-package :core)
(part-of XWINDOWS)

;;;
;;;File: PRFW
;;;Package: XWINDOWS
;;;Defines proofwindows for use by those using xwindows

(deffile prfw
  (part-of xwindows)
  (extension lisp)
  (mhelp "Defines proofwindows for use by those using xwindows."))

(context subtoplevels)

(deftoplevel prfw-top
  (top-prompt-fn prfw-tps-top-prompt)
  (command-interpreter interpret-prfw-top)
  (print-* print-*-tps-top)
  (top-level-category mexpr)
  ;;(top-level-ctree command-ctree)
  (top-cmd-decode prfw-comdecode)
  (mhelp "The command top level of TPS, supplemented by proofwindow output."))

(defun prfw-tps-top-prompt (id)
  (format nil "<PRFW~A>" id))

; Commands/Rules listed here cause the prfw to be updated
; when they are executed/applied
(defvar *prfw-iftrue-update-commands*
  (list 'add-hyps 'delete-hyps ; cebrown 8/20/99
	'pall 'cleanup 'make-assert-a-hyp 'line-comment 'proof-comment 'transfer-lines 'merge-proofs ; cebrown 6/24/04
        'exercise 'prove 'restoreproof 'reconsider 'create-subproof
	'save-subproof 'merge-proofs
	'ml::delete 'ml::delete* 'ml::introduce-gap 
	'ml::modify-gaps 'ml::move 'ml::move* 
	'ml::plan 'ml::renumberall 'ml::squeeze  
        'ml::pstatus 'ml::sponsor 'ml::subproof 'ml::unsponsor 
	'ml::assert 'ml::hyp 'ml::lemma 'ml::same 
	'ml::cases 'ml::deduct 'ml::disj-imp 'ml::econj 'ml::equiv-implics 
	'ml::iconj 'ml::imp-disj 'ml::implics-equiv 'ml::indirect 
	'ml::indirect1 
	'ml::indirect2 'ml::mp 
	'ml::absurd 'ml::eneg 'ml::ineg 'ml::pullneg 'ml::pushneg 
	'ml::ab* 'ml::abe 'ml::abu 'ml::egen 'ml::rulec 'ml::ugen 'ml::ui 
	'ml::substitute 
	'ml::ext= 'ml::ext=0 'ml::let 'ml::subst= 'ml::subst=l 'ml::subst=r 
	'ml::sym= 'ml::edef 'ml::equiv-wffs 'ml::equiv-eq 'ml::idef 
	'ml::lambda* 'ml::lcontr* 'ml::lexpd* 'ml::lcontr*-beta 'ml::lcontr*-eta
	'ml::lambda*-beta 'ml::lambda*-eta 'ml::lexpd*-beta 'ml::lexpd*-eta 'ml::rulep 'ml
	; mkaminski 11/8/2005
	'ml::assert2
	'app 'prove-in 'rewrite 'rewrite-in 'beta-nf 'app* 'unapp* 'connect
	'beta-eq 'eta-nf 'eta-eq 'lambda-nf 'lambda-eq 'long-eta-nf 'rewriting
	'same 'assert-top 'any 'any* 'unany* 'derive 'derive-in 'auto
	'any*-in 'unany*-in
	)
  "Commands after which the proofwindows should be updated, if the result returned is T")

(defvar *prfw-pall-window* nil) ; will be the name of the stream for pall
(defvar *prfw-^p-window* nil)
(defvar *prfw-^pn-window* nil)
(defvar *prfw-pall-process* nil) ; the number of the xterm process
(defvar *prfw-^p-process* nil)
(defvar *prfw-^pn-process* nil)
(defvar *big-proofwindow* nil) ; size of proofwindow

(defflag proofw-all
  (flagtype boolean)
  (default t)
  (subjects otl-vars printing window-props)
  (mhelp "If T, entire proof so far is printed in the Complete Proof 
window, if this window exists."))

(defflag proofw-active
  (flagtype boolean)
  (default t)
  (subjects otl-vars printing window-props)
  (mhelp "If T, active lines of the current proof are printed in the 
Current Subproof window, if this window exists."))

(defflag proofw-active+nos
  (flagtype boolean)
  (default t)
  (subjects otl-vars printing window-props)
  (mhelp "If T, active lines of the current proof are printed in the 
Current Subproof & Line Numbers window, if this window exists."))

(defflag proofw-active-height
  (flagtype posinteger)
  (default 24)
  (subjects otl-vars window-props)
  (mhelp "Controls the initial height of the Current Subproof window."))

(defflag proofw-all-height
  (flagtype posinteger)
  (default 24)
  (subjects otl-vars window-props)
  (mhelp "Controls the initial height of the Complete Proof window."))

(defflag proofw-active-width
  (flagtype posinteger)
  (default 80)
  (subjects otl-vars window-props)
  (mhelp "Controls the initial width of the Current Subproof window."))

(defflag proofw-all-width
  (flagtype posinteger)
  (default 80)
  (subjects otl-vars window-props)
  (mhelp "Controls the initial width of the Complete Proof window."))

(defflag proofw-active+nos-height
  (flagtype posinteger)
  (default 24)
  (subjects otl-vars window-props)
  (mhelp "Controls the initial height of the Current Subproof & Line Numbers
window."))

(defflag proofw-active+nos-width
  (flagtype posinteger)
  (default 80)
  (subjects otl-vars window-props)
  (mhelp "Controls the initial width of the Current Subproof & Line Numbers
window."))

(defmexpr begin-prfw
  (argtypes)
  (argnames )
  (mhelp "Begin proofwindow top level.
Open Current Subproof, Current Subproof & Line Numbers, and Complete Proof
windows with text size determined by the value of the flag CHARSIZE.
Printing in various windows can be modified by changing the flags
PROOFW-ACTIVE, PROOFW-ALL, PROOFW-ACTIVE+NOS, BLANK-LINES-INSERTED 
and PRINTLINEFLAG.
The initial size of the windows can be modified with the flags
PROOFW-ALL-HEIGHT, PROOFW-ALL-WIDTH, PROOFW-ACTIVE-HEIGHT,
PROOFW-ACTIVE-WIDTH, PROOFW-ACTIVE+NOS-HEIGHT, and 
PROOFW-ACTIVE+NOS-WIDTH; after the windows are open, they can simply 
be resized as normal. PSTATUS will update the proofwindows manually
if necessary. 
Close the proofwindows with END-PRFW."))

(defmexpr end-prfw
  (argtypes)
  (argnames )
  (mhelp "End proofwindow top level; close all open proofwindows."))

;;;The following function is used to get rid the annoying "NIL" and "T",
;;;which always follow a command otherwise.
(defun prfw-print-* (result) (declare (ignore result)))

(defun general-begin-prfw ()
;;; set up *prfw-pall-window* and *prfw-^p-window* as streams to be
;;; printed to, then begin a new top level
  (declare (special printlineflag *using-interface*))
  (unless (or *prfw-pall-window* *prfw-^p-window* *prfw-^pn-window*)          ; prfw already started.
    (if proofw-all (setq *prfw-pall-window* (setup-prfw-pall-window)) (setq *prfw-pall-window* nil))
    (if proofw-active (setq *prfw-^p-window* (setup-prfw-^p-window)) (setq *prfw-^p-window* nil))
    (if proofw-active+nos (setq *prfw-^pn-window* (setup-prfw-^pn-window)) (setq *prfw-^pn-window* nil))
    (setq printlineflag nil)
    (unwind-protect 
	(let ((command-interpreter #'interpret-prfw-top)
              (print-* #'prfw-print-*)
              (top-prompt-fn #'prfw-tps-top-prompt))
	  (%catch% (progn (secondary-top)
			  (throwfail "Prfw aborted."))
		   (exit-inferior-top)))
      (if *using-interface*
	  (progn
	    (close-window *prfw-pall-window*)
	    (close-window *prfw-^p-window*)
	    (close-window *prfw-^pn-window*)
	    (setq *prfw-pall-window* nil *prfw-^p-window* nil *prfw-^pn-window* nil))
	(progn
	  (when (streamp *prfw-pall-window*)
	    (close *prfw-pall-window*) 
	    (if (probe-file (pathname *prfw-pall-window*)) (delete-file (pathname *prfw-pall-window*))))
	  (when (streamp *prfw-^p-window*)
	    (close *prfw-^p-window*)  
	    (if (probe-file (pathname *prfw-^p-window*)) (delete-file (pathname *prfw-^p-window*))))
	  (when (streamp *prfw-^pn-window*)
	    (close *prfw-^pn-window*)  
	    (if (probe-file (pathname *prfw-^pn-window*)) (delete-file (pathname *prfw-^pn-window*))))
	  (if *prfw-pall-window* (setq *prfw-pall-window* (kill-xterm-window *prfw-pall-process* )))
	  (if *prfw-^pn-window* (setq *prfw-^pn-window* (kill-xterm-window *prfw-^pn-process* )))
	  (if *prfw-^p-window* (setq *prfw-^p-window* (kill-xterm-window *prfw-^p-process* ))))))))

;;; The functions setup-xterm-window and kill-xterm-window have been moved to tops20.lisp, 
;;; since they are system-dependent.
;;; They currently run on cmulisp and Allegro Common Lisp only.

(defun begin-prfw ()
  (progn (if (equal charsize 'MAX)
	     (setq *big-proofwindow* T)
	   (setq *big-proofwindow* NIL))
	 (general-begin-prfw)))

;;; this is arcane and silly, and is only written like this for historical reasons!
;;; there are much more efficient ways to do this, but efficiency isn't an issue here...

(defun setup-prfw-^p-window ()
  (declare (special *using-interface*))
  "Make a new window to which we can output ^P as a Lisp stream."
  (if *using-interface*
      (open-window-with-socket "prfw^p" "Current Subproof"
			       proofw-active-width proofw-active-height
			       *big-proofwindow*)
    (let* ((outfilename (concatenate 'string "/tmp/foo" 
				     (princ-to-string (* 3 (tps-get-internal-run-time)))))
	   (outstream (open outfilename :direction :output 
			    :if-exists :overwrite 
			    :if-does-not-exist :create)))
      (if *big-proofwindow*
	  (setq *prfw-^p-process* 
		(setup-big-xterm-window "Current Subproof" outfilename 
					(concatenate 'string (princ-to-string proofw-active-width) "x" 
						     (princ-to-string proofw-active-height) "+0-0")))
	(setq *prfw-^p-process* 
	      (setup-xterm-window "Current Subproof" outfilename 
				  (concatenate 'string (princ-to-string proofw-active-width) "x" 
					       (princ-to-string proofw-active-height) "+0-0"))))
      outstream)))

(defun setup-prfw-^pn-window ()
  (declare (special *using-interface*))
  "Make a new window to which we can output ^PN as a Lisp stream."
  (if *using-interface*
      (open-window-with-socket "prfw^pn" "Current Subproof & Line Numbers"
			       proofw-active-width proofw-active-height
			       *big-proofwindow*)
    (let* ((outfilename (concatenate 'string "/tmp/foo" 
				     (princ-to-string (* 5 (tps-get-internal-run-time)))))
	   (outstream (open outfilename :direction :output 
			    :if-exists :overwrite 
			    :if-does-not-exist :create)))
      (if *big-proofwindow*
	  (setq *prfw-^pn-process* 
		(setup-big-xterm-window "Current Subproof & Line Numbers" outfilename 
					(concatenate 'string (princ-to-string proofw-active-width) "x" 
						     (princ-to-string proofw-active-height) "-0+0")))
	(setq *prfw-^pn-process* 
	      (setup-xterm-window "Current Subproof & Line Numbers" outfilename 
				  (concatenate 'string (princ-to-string proofw-active-width) "x" 
					       (princ-to-string proofw-active-height) "-0+0"))))
      outstream)))

(defun setup-prfw-pall-window ()
  (declare (special *using-interface*))
  "Make a new window to which we can output PALL as a Lisp stream."
  (if *using-interface*
      (open-window-with-socket "prfw-pall" "Complete Proof"
			       proofw-all-width proofw-all-height
			       *big-proofwindow*)
    (let* ((outfilename (concatenate 'string "/tmp/foo" 
				     (princ-to-string (get-universal-time))))
	   (outstream (open outfilename :direction :output 
			    :if-exists :overwrite 
			    :if-does-not-exist :create)))
      (if *big-proofwindow*
	  (setq *prfw-pall-process* 
		(setup-big-xterm-window "Complete Proof" outfilename 
					(concatenate 'string (princ-to-string proofw-all-width) "x" 
						     (princ-to-string proofw-all-height) "+0+0")))
	(setq *prfw-pall-process* 
	      (setup-xterm-window "Complete Proof" outfilename 
				  (concatenate 'string (princ-to-string proofw-all-width) "x" 
					       (princ-to-string proofw-all-height) "+0+0"))))
      outstream)))

(defun end-prfw ()
  (declare (special printlineflag))
  (progn (setq printlineflag T)
	 (%throw% '|[Normal Exit of Prfw Toplevel.]| exit-inferior-top)))


(defun prfw-comdecode (command)
  "Use comdecode on the command, and then update the prfw windows if
desired."
  (declare (special global-srulelist))
  (let ((command-name (car command))
	(result (comdecode command)))
    (when (and result (or (member command-name *prfw-iftrue-update-commands*) (member command-name global-srulelist)))
; this way we don't have to remember to add new rules to *prfw-iftrue-update-commands* ... MB 8/23/93
	  (prfw-pall)
	  (prfw-^p)
	  (prfw-^pn)
	  result)))

;;; the flag blank-lines-inserted is also used by the edwindows, and is defined in wffout.lisp

(defun interpret-prfw-top (cmd)
  "Same as interpret-tps-top, but uses prfw-comdecode instead of comdecode."
  (setq curr-cmd 'none)
  (cond ((null cmd) nil)
	;; Input of form '(atom)
	((and (null (cdr cmd)) (atom (car cmd)))
	 (cond ;; Have to make sure we have a symbol before
	       ;; doing these gets
	       ((symbolp (car cmd))
		(cond 
		 ((get (car cmd) 'mexpr)
		  (setq curr-cmd (car cmd))
		  `(prfw-comdecode ',cmd))	       
		 ((get (car cmd) 'reviewcmd)
		  (if (eq (car cmd) 'leave)
		      (progn (complain "Use END-PRFW to leave.") nil)
		      `(prfw-comdecode ',cmd)))
		 ((get (car cmd) 'flag)
		  `(prfw-comdecode '(setflag ,@cmd)))
		 ((null expertflag)
		  (throwfail "Unknown Command." ))
		 ((or (get (car cmd) 'mhelp) (get (car cmd) 'mhelp-fn))
		  (msg "Cannot evaluate that... calling HELP " (car cmd) t t)
		  `(comdecode '(help ,(car cmd))))
		 ((boundp (car cmd)) `(multiple-prin1 ,(car cmd)))
		 ((not (fboundp (car cmd)))
		  (throwfail ";" (car cmd) " - Unbound variable." ))))
	       ;; Not a symbol 
	       ((null expertflag)
		(throwfail "Unknown Command." ))
	       (t `(multiple-prin1 ,(car cmd)))))
	((and expertflag (null (cdr cmd)))
	 (cond ((not (and (symbolp (caar cmd)) (fboundp (caar cmd))))
		(throwfail ";" (car cmd) " - Undefined function." ))
	       (t `(multiple-prin1 ,(car cmd)))))
	((not (symbolp (car cmd)))
	 (throwfail "Illegal input - must be EXPERT to evaluate Lisp form." ))
	((get (car cmd) 'mexpr)
	 (setq curr-cmd (car cmd))
	 `(prfw-comdecode ',cmd))
	((get (car cmd) 'reviewcmd)
	 `(prfw-comdecode  ',cmd))
	((get (car cmd) 'flag)
	 (if (cdr cmd) `(prfw-comdecode '(set ,@cmd))
	     `(prfw-comdecode '(setflag ,@cmd))))
	((null expertflag)
	 (throwfail "Unknown command." t))
	((not (fboundp (car cmd)))
	 (throwfail ";" (car cmd) " - Undefined function." ))
	(t `(multiple-prin1 ,cmd))))
