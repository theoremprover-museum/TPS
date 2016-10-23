;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1990 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of ms90-3)

(deffile ms90-3-node
  (part-of ms90-3)
  (mhelp "Definitions, Functions, etc., needed by unification, mating search,
	 etc. Version ms90-3. Implementation of Path-focused duplication."))

(defstruct (unode (:print-function print-unode))
  (dpairs nil)
  (sons nil)
  (substs nil)
  (depth 0)
  (fo-mode nil)
  (msv nil))

(defun print-unode (unode *standard-output* *print-level*)
  (format t "Dpairs: ~A~%Substs: ~A~%~A ~A" (unode-dpairs unode)
          (unode-substs unode) (length (unode-sons unode))
          (unode-depth unode)))

;(defparameter eta-var-prefix (intern-str "n"))
;(defparameter initial-value-h-var-counter 1)
;(defparameter initial-value-eta-var-counter 1)
;these are already in node.lisp

(defflag num-of-dups
  (flagtype integer+)
  (default 2)
  (subjects important mating-search ms90-3 ms90-9 ms91-7 ms92-9 ms93-1 ms98-1 transmit)
  (mhelp "Max number of duplications allowed on any path in 
    search procedures using path-focused duplication.
    This flag may be set to 0."))

(defconstnt default-env-stack '((var . -1)))
(defconstnt bkup-disj '^)
(defconstnt pre-conn '^)


(context ms90-3)

(defflag show-time
  (flagtype boolean)
  (subjects mating-search ms90-3 ms90-9 ms91-7 ms92-9 ms93-1 transmit)
  (default t)
  (mhelp "When true, print the time taken by MS90-3 and MS90-9."))

(defflag num-frpairs
  (flagtype integer+)
  (subjects unification ms90-3 ms90-9 ms91-7 ms92-9 ms93-1 transmit)
  (default 5)
  (mhelp "The match routine considers at most NUM-FRPAIRS frpairs,
before selecting a frpair. However, if it finds a pair that has at
most 1 substitution, it will automatically select this pair.
Applies to UN90 only."))

(defmateop ms90-3
  (mate-alias ms90-3-real) ;was ms90-3-controller
  (mhelp "Start mating search procedure MS90-3 on current eproof.
This search procedure incorporates Issar's path-focused duplication,
but works on just one jform at a time.  Only duplications are done,
not primitive substitutions.  This is not an interactive procedure."))

(defmateop prop-msearch
  (mate-alias mate-prop-msearch) 
  (mhelp "Start Sunil's propositional mating search procedure.
This search procedure only works on propositional jforms."))

(defun mate-prop-msearch ()
  (declare (special max-mates))
  (startcount 'mating)
  (let ((default-ms 'ms90-3)
	(max-mates 100000)
	(order-components t)
	(connxxlist nil))
    (declare (special max-mates))
    (setq active-mating (init-mating))
    (setq connxxlist (prop-msearch (cr-eproof-jform)))
    (if (eq connxxlist 'fail)
	(progn (breakcount 'mating) 
	       (msgf t "PROP-MSEARCH failed because either current eproof is not propositional, 
or it is not provable." t))
      (progn (dolist (conn connxxlist) (add-conn (princ-to-string (car conn)) (princ-to-string (cdr conn))))
	     (setf (mating-completep active-mating) t)
	     (msgf t "Found a complete mating." t)
	     (breakcount 'mating) (display-time 'mating)
	     nil))))

(defun mate-prop-msearch-silent ()
  (declare (special max-mates))
  (startcount 'mating)
  (let ((default-ms 'ms90-3)
	(max-mates 100000)
	(order-components t)
	(connxxlist nil)
	(closed nil))
    (declare (special max-mates))
    (init-mating)
    (multiple-value-setq (connxxlist closed) (prop-msearch-silent (cr-eproof-jform)))
    (if (eq closed 'fail)
        (progn (breakcount 'mating) 'fail)
      (progn (dolist (conn connxxlist) (add-conn (princ-to-string (car conn)) (princ-to-string (cdr conn))))
	     (setf (mating-completep active-mating) t)
	     (msgf t "Found a complete mating." t)
	     (breakcount 'mating) (display-time 'mating)
	     nil))))

(defun prop-msearch-silent (jform)
  (let ((*cgraph2* (make-hash-table :test #'eq))
        (*cgraph* (make-hash-table :test #'eq))
        (temp-list nil))
    (declare (special *cgraph* *cgraph2* temp-list))
    (setq jform (cnvrt-prop-jform jform nil))
    (prop-setup-cgraph jform)
    (multiple-value-bind (conn path)
	(prop-find-cheapest-path jform nil)
      (do ((mating (list conn)
                   (if (eq conn t) mating (cons conn mating))))
	  ((not (and path conn))
           (if path (values path 'fail)
               (values (cdr mating) jform)))
        (multiple-value-setq (conn path)
          (prop-find-alt-path (car path) (cdr path) t nil))))))

(defun ms90-3-real ()
  (let ((default-ms 'ms90-3))
    (matingsearch-controller)))

(defvar dup-record nil)
(defvar ms90-3-jform nil)
(defvar unif-prob nil)

(defun ms90-3-controller (&optional
                          (etree (eproof-etree current-eproof))
			  (time-limit nil))
;;;Next line is added for the purpose of MAX-DUP-PATHS.
  (declare (special ms90-3-jform))
  (setq ms90-3-jform (init-position (etree-to-jform etree)))
  (pathnum-init-jform ms90-3-jform)
  (when (memq mating-verbose '(max med))
    (msg T "Displaying VP diagram ..." t)
    (when (eq style 'concept-s) (tyol 85) (tyol 106 32))
    (display-vp-diag ms90-3-jform))
  (remove-sk-labels ms90-3-jform)
  (if show-time 
      (let ((*trace-output* *standard-output*))
	(time (multiple-value-setq (dup-record ms90-3-mating unif-prob)
		(msearch ms90-3-jform time-limit))))
      (multiple-value-setq (dup-record ms90-3-mating unif-prob)
        (msearch ms90-3-jform time-limit)))
  (if (and *test-top* (eq dup-record 'test-fail)) 
      (values 'test-fail nil)
    (if time-limit
	(if dup-record			; mating-found
	    (values 'succeed nil)
	  (values 'fail nil))
      dup-record)))

(defun remove-sk-labels (jform)
  (case (jform-type jform)
    (literal (setf (jform-represents jform)
                   (remove-sk-labels-wff (jform-represents jform))))
    (disjunction (mapc #'remove-sk-labels (disjunction-components jform)))
    (conjunction (mapc #'remove-sk-labels (conjunction-components jform)))
    (universal (remove-sk-labels (universal-scope jform)))))

(defun remove-sk-labels-wff (gwff)
  (cond ((label-q gwff)
         (apply-label gwff (remove-sk-labels-wff gwff)))
        ((lsymbol-q gwff) gwff)
        (t (cons (remove-sk-labels-wff (car gwff))
                 (remove-sk-labels-wff (cdr gwff))))))
         

(defflag ms90-3-dup-strategy
  (flagtype integer+)
  (default 1)
  (subjects mating-search ms89 ms90-3 ms90-9 ms91-7 ms92-9 ms93-1 transmit)
  (mhelp "1 to select any combination of duplications (2 1 3 1 is
allowed), any thing else to select duplications in non decreasing
order only. (2 1 3 1 is not allowed, but 1 1 2 3 is allowed.)")) 

(defvar neg-h-var-list nil)
;;;;;;don't generate negation-imitations for these variables.
(defvar imitation-h-var-list nil)
;;;;;;generate only imitations for these variables.

(defflag print-mating-counter
  (flagtype integer+)
  (default 300000)
  (subjects mating-search ms90-3 ms90-9 ms91-7 transmit)
  (mhelp "Prints the current mating after this many iterations in the
top level ms90-3 search. Applicable only for path-focused duplication
search procedures")) 

(defflag max-mates
  (flagtype posinteger-or-infinity)
  (default 2)
  (subjects mating-search ms90-3 ms90-9 ms91-7 ms91-6 ms89 ms88 ms92-9 ms93-1 ms98-1 important transmit)
  (mhelp "Max number of mates for a literal. If the search attempts to
add a mate that would exceed this limit, then this connection is not added.
Copies of a literal created by path-focused duplication are regarded as
the same when computing this number.
Set MAX-MATES to INFINITY to allow an unlimited number of mates for any 
literal."))

