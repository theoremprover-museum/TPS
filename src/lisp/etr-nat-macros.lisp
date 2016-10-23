;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)
(part-of etr-nat)

(context etr-nat)
(deffile etr-nat-macros
  (extension lisp)
  (part-of etr-nat)
  (mhelp "Functions and macros needed for translating from expansion
trees to natural deduction proofs."))

(defflag etree-nat-verbose
  (flagtype print-function-list)
  (default (prfw-pall prfw-^p prfw-^pn ^pn))
  (subjects etr-nat printing window-props transmit)
  (mhelp "Should be a list of print-functions (see the help message 
for PRINT-FUNCTION), which will be executed after each tactic during
ETREE-NAT."))

(defmexpr tidy-proof
  (argtypes symbol symbol)
  (argnames old-prfname new-prfname)
  (arghelp "Name of old proof" "Name for new proof")
  (defaultfns (lambda (s1 s2) 
		(list (if (eq s1 '$) dproof s1) s2)))
  (mhelp "Translate a ND proof to an eproof and back again 
(into a proof with a new name) in the hope of tidying it up a 
bit. Equivalent to
NAT-ETREE; MATE ! ; PROP-MSEARCH ; MERGE-TREE ; LEAVE ; 
ETREE-NAT ; CLEANUP ; SQUEEZE"))

(defun tidy-proof (oldp newp)
  (unless (eval (interpret-tps-top (list 'nat-etree oldp)))
	  (throwfail t "TIDY-PROOF failed because NAT-ETREE didn't finish." t))
  (mate-wff-prefix current-eproof t nil)
  (mate-prop-msearch)
  (complete-p)
  (unless (mating-completep active-mating)
	  (throwfail t "TIDY-PROOF failed because PROP-MSEARCH failed." t))
  (merge-tree)
  (eval (interpret-tps-top (list 'etree-nat newp '!)))
  (cleanup)
  (remove-unnecessary-gaps)
  (pall))

(defmexpr etree-nat 
 (argtypes symbol line tactic-exp tactic-mode)
 (argnames prefix num tac mode)
 (arghelp "Name of the Proof" "Line Number for Theorem" "Tactic to be Used" "Tactic Mode")
 (defaultfns (lambda (x y z w)
	       (list
		(if (eq '$ x) dproof x)
		(if (eq '$ y) 100 y)
		(if (eq '$ z) default-tactic z)
		(if (eq '$ w) 'auto w))))
 (mhelp "Translates the current expansion proof, which is value of internal
variable current-eproof, into a natural deduction style proof. The default
value of the tactic is given by the flag DEFAULT-TACTIC."))

;;; Merging of the expansion proof should have already been done before
;;; this is called.

(defun etree-nat (prefix num tac mode &optional (eproof current-eproof))
  (unwind-protect 
    (etree-nat-real prefix num tac mode eproof)
    (progn (breakcount 'eproof) (display-time 'eproof))))

(defun etree-nat-real (prefix num tac mode &optional (eproof current-eproof))
  (declare (special ASSERT-LEMMAS))
  (startcount 'eproof);hx: Dec. 10, 1992
  (when (and (not tac) (neq query-user T)) ;changed MB 7/23/93: was (not query-user)
    (setq tac default-tactic))
  (unless tac
    (prompt-read tac nil (msgf "What tactic should be used for translation?")
		 'tactic-exp default-tactic nil))
  (when (and (not mode) (neq query-user T)) ;changed MB 7/23/93 : was  (not query-user)
    (setq mode 'auto))
  (unless mode
    (prompt-read mode nil (msgf "Tactic mode?")
		 'tactic-mode 'auto ((? (mhelp 'tactic-mode)))))
  (if (and (eproof-p eproof)
	   (etree-p (eproof-etree eproof))
	   (boundp 'active-mating)
	   (mating-p active-mating)
	   (mating-completep active-mating))
  (let ((line (gensym))
	(linewff nil)
	(neglemmas nil)
	(poslemmas nil)
	(etree (eproof-etree eproof))
	(mating nil)
	(wff nil))
    (setq dproof prefix)
    (if (numalias num) ; line already in proof
	(progn
	  (setq line (numalias num))
	  (setq linewff (line-assertion line))))
    (when (eproof-lemmas eproof)
      (setq neglemmas (car (etree-components etree)))
      (setq poslemmas (car (etree-components (cadr (etree-components etree)))))
      (setq etree (cadr (etree-components (cadr (etree-components etree)))))
      (when (and linewff (not (wffeq linewff (get-shallow etree))))
	(throwfail "Shallow formula of expansion proof does not
match that of line " (line . existing-line) ".")))
    (setq wff (get-shallow etree))
    (when (null (proof-lines dproof)) ; not already a proof in progress
      (setf (proof-assertion dproof) wff)
      (setf (proof-lines dproof) nil)
      (setf (proof-linealiases dproof) nil)
      (setf (proof-plans dproof) nil)
      (setf (nextplan-no dproof) 1))
    (setq core::*dproof-list* (adjoin dproof core::*dproof-list*))
    (setf (line-justification line) (nextplan))
    (setf (line-assertion line) wff)
    (setf (line-linenumber line) num)
    (setf (line-node line) etree)
    (setq mating
	  (mapcar #'(lambda (x) 
		      (if (numberp x)
			  (let ((conn (car (gethash x (connections-array)))))
			    (cons (literal-name (car conn)) 
				  (literal-name (cdr conn))))
			  (cons (if (etree-p (car x))
				    (etree-name (car x))
				    (literal-name (car x)))
				(if (etree-p (cdr x))
				    (etree-name (cdr x))
				    (literal-name (cdr x))))))
		  (mating-clist active-mating)))
    (setf (line-mating line) mating)
    (when (not (memq line (proof-lines dproof)))
      (setf (proof-linealiases dproof) 
	    (merge 'list (list (cons num line))
		   (proof-linealiases dproof)
		   #'< :key #'car))
      (setf (proof-lines dproof) 
	    (merge 'list (list line) (proof-lines dproof) #'<
		   :key #'linealias)))
    (when (not (assoc line (proof-plans dproof)))
      (setf (proof-plans dproof) 
	    (push (list line) (proof-plans dproof))))
    (if ASSERT-LEMMAS
	(etr-nat-assert-lemmas line neglemmas poslemmas ; cebrown, 10/20/01
			       (eproof-lemmas eproof) tac mode)
      (etr-nat-introduce-lemmas line neglemmas poslemmas ; cebrown, 10/18/01
				(eproof-lemmas eproof) tac mode))
    (find-all-gaps)
    (apply-tactic tac :use 'etree-nat :mode mode :goal line)
    ;(prfw-pall) superseded
    ;(prfw-^p) superseded
    (print-routines)
    (runcount 'eproof))
  (throwfail "ETREE-NAT is not applicable.")))

(defun print-routines (&optional (xxx etree-nat-verbose))
  (if (null xxx) t
    (progn
      (case (car xxx)
	    (^p (prtactive))
	    (^pn (prtactivenos))
	    (pall (terpri) (prtlines (get dproof 'lines)))
	    (pstatus (prtstatus))
	    (prfw-pall (prfw-pall))
	    (prfw-^p (prfw-^p))
	    (prfw-^pn (prfw-^pn))
	    (t (msg "Unknown print function; get a TPS maintainer to update the 
type of print-functions" t)))
      (print-routines (cdr xxx)))))

(defun mated-p (pline)
  (mated-p* (line-node pline) (line-mating pline)))

(defun mated-to (line1 line2 conn-list)
  "Returns T only if the etrees represented by line1 and line2
(natural deduction proof lines) are mated to each other by some 
connection in conn-list."
  (mated-to-node (line-node line1) (line-node line2) conn-list))

(defun mated-to-node (node1 node2 conn-list)
  "Returns T only if the node1 and node2 are mated to each other
by some connection in conn-list."
  (let ((name1 (etree-name node1))
	(name2 (etree-name node2)))
    (dolist (conn conn-list nil)
      (if (or (and (string-equal name1 (car conn))
		   (string-equal name2 (cdr conn)))
	      (and (string-equal name1 (cdr conn))
		   (string-equal name2 (car conn))))
	  (return t)))))

(defun restrict-mating-to (conn-list node-list)
  "Returns connections from the conn-list both of whose ends are
below some node in the given node-list."
  (let ((new-conn-list nil)
	(and-node (make-econjunction :components node-list)))
    (dolist (conn conn-list new-conn-list)
      (when (and (find-etree-node-name (car conn) and-node t)
		 (find-etree-node-name (cdr conn) and-node t))
	(push conn new-conn-list)))))

(defun restrict-mating-from (conn-list extra-node)
  "Returns conn-list minus those connections involving nodes below 
extra-node."
  (let ((new-conn-list nil))
    (dolist (conn conn-list new-conn-list)
      (unless (or (find-etree-node-name (car conn) extra-node t)
		  (find-etree-node-name (cdr conn) extra-node t))
	(push conn new-conn-list)))))

(defun inessential-p (mating etree)
  "Returns T iff the etree is not used in the mating."
  (dolist (conn mating t)
    (when (or (find-etree-node-name (car conn) etree t)
	      (find-etree-node-name (cdr conn) etree t))
      (return nil))))

;;;There is a glitch in the following function. Consider
;;;formula EXISTS X. A, where X does not occur in A.
;;;Obviously, such nodes should not be counted. The fix
;;;does exactly this.
#+comment(defun admissible-p (supports term)
  (let* ((skol-nodes (mapcan 
		     #'(lambda (x) (find-etree-nodes 
				    #'(lambda (y) (or (skolem-p y)
						      (selection-p y)))
				    x nil t))
		     supports))
	 (params (mapcar #'(lambda (x) (if (skolem-p x)
					   (car (skolem-terms x))
					   (car (selection-terms x))))
			 skol-nodes)))
    (null (setintersect (free-vars-of term) params))))

(defun admissible-p (supports term)
  (let* ((skol-nodes (mapcan 
		     #'(lambda (x) (find-etree-nodes 
				    #'(lambda (y) 
                                          (or (and (skolem-p y) 
                                                   (free-in (caar (skolem-shallow y))
                                                            (cdr (skolem-shallow y))))
					      (and (selection-p y)
                                                   (free-in (caar (selection-shallow y))
                                                            (cdr (selection-shallow y))))))
				    x nil t))
		     supports))
	 (params (mapcar #'(lambda (x) (if (skolem-p x)
					   (car (skolem-terms x))
					   (car (selection-terms x))))
			 skol-nodes)))
    (null (setintersect (free-vars-of term) params))))

(defun spans (conclusion supports conn-list)
  "Conclusion is an etree node, supports is a list of etree nodes, conn-list
a list of connections.  Returns T if conn-list is a spanning mating on
the etree constructed by making an implication node with left child the
conjunction of the supports, and with the right child being conclusion." 
  (let* ((topnode (make-implication :positive nil :junctive 'con))
	 (left (cond ((cdr supports)
		      (make-econjunction :positive t :junctive 'con
					 :components supports))
		     (supports (car supports))
		     (t (make-true :positive t)))))
    (declare (special *fps-succeeded*))
    (update-status nil left 1)
    (update-status nil topnode 1)
    (update-status nil conclusion 1)
    (setf (etree-components topnode)
	  (list left conclusion))
    (multiple-value-prog1
	(old-spanning-clist-p (etree-to-prop-jform topnode) conn-list) ; to force it to consider conn-list - cebrown 3/15/01
      )))

; lemmas - cebrown 10/18/01
(defun etr-nat-introduce-lemmas (pline neglems poslems lemmas tac mode)
  (when lemmas
    (unless (inessential-p (line-mating pline)
			   (if (cdr lemmas)
			       (car (etree-components poslems))
			     poslems))
      (if (numalias 1)
	  (introduce-gap (numalias 1) 2)
	(if (numalias 2)
	    (introduce-gap (numalias 2) 1)))
      (let* ((poslem (if (cdr lemmas)
			 (car (etree-components poslems))
		       poslems))
	     (neglem (if (cdr lemmas)
			 (car (etree-components neglems))
		       neglems))
	     (sh (get-shallow poslem)))
	(comdecode (list 'cl-user::lemma (linealias pline) 2 (list 'quote sh)
			 '$ '$ nil))
	(comdecode (list 'cl-user::same 2 1 '$ '$ '$))
	(let ((lpline (numalias 1))
	      (lsline (numalias 2)))
	  (setf (line-mating lpline) (line-mating pline))
	  (setf (line-node lsline) poslem)
	  (setf (line-node lpline) neglem)
	  (when (cdar lemmas)
	    (let ((lneglems (car (etree-components neglem)))
		  (lposlems (car (etree-components (cadr (etree-components neglem))))))
	      (setf (line-node lpline)
		    (cadr (etree-components (cadr (etree-components neglem)))))
	      (etr-nat-introduce-lemmas
	       lpline lneglems lposlems (cdar lemmas) tac mode)))
	  (apply-tactic tac :use 'etree-nat :mode mode :goal lpline))))
    (when (cdr lemmas)
      (etr-nat-introduce-lemmas pline
				(cadr (etree-components neglems))
				(cadr (etree-components poslems))
				(cdr lemmas) tac mode))))

(defun etr-nat-assert-lemmas (pline neglems poslems lemmas tac mode)
  (when lemmas
    (let ((save-dproof dproof)
	  (poslem (if (cdr lemmas)
		      (car (etree-components poslems))
		    poslems)))
      (unless (inessential-p (line-mating pline) poslem)
	(when (numalias 1)
	  (introduce-gap (numalias 1) 1))
	(let* ((neglem (if (cdr lemmas)
			   (car (etree-components neglems))
			 neglems))
	       (sh (get-shallow poslem))
	       (lpf (caar lemmas)) ; symbol giving the name of the lemma
	       (lline (gensym)))
	  (comdecode (list 'cl-user::lemma (linealias pline) 1 (list 'quote sh)
			   '$ '$ nil))
	  (setf (line-justification (numalias 1)) (list (format nil "Assert: ~d" lpf)
							nil nil))
	  (setf (proof-plans dproof) 
		(remove (list (numalias 1)) (proof-plans dproof) :test #'equal))
	  (setf (line-node (numalias 1)) poslem)
	  (setq dproof lpf)
	  (setf (proof-assertion dproof) sh)
	  (setf (proof-lines dproof) nil)
	  (setf (proof-linealiases dproof) nil)
	  (setf (proof-plans dproof) nil)
	  (setf (nextplan-no dproof) 1)
	  (push lpf core::*dproof-list*)
	  (setf (line-justification lline) (nextplan))
	  (setf (line-assertion lline) sh)
	  (setf (line-linenumber lline) 100)
	  (setf (line-node lline) neglem)
	  (setf (line-mating lline) (line-mating pline))
	  (setf (proof-linealiases dproof) (acons 100 lline nil))
	  (setf (proof-lines dproof) (list lline))
	  (push (list lline) (proof-plans dproof))
	  (when (cdar lemmas) ; the lemma depends on other lemmas
	    (let ((lneglems (car (etree-components neglem)))
		  (lposlems (car (etree-components (cadr (etree-components neglem))))))
	      (setf (line-node lline)
		    (cadr (etree-components (cadr (etree-components neglem)))))
	      (etr-nat-assert-lemmas
	       lline lneglems lposlems (cdar lemmas) tac mode)))
	  (find-all-gaps)
	  (apply-tactic tac :use 'etree-nat :mode mode :goal lline)
	  (unless (proof-plans dproof)
	     ; cebrown 6/18/2002 - make-nice, but don't call PALL unless printlineflag is T
	    (cleanup) (remove-unnecessary-gaps)
	    (when printlineflag (pall)))
	  (setq dproof save-dproof))))
    (when (cdr lemmas)
      (etr-nat-assert-lemmas pline
			     (cadr (etree-components neglems))
			     (cadr (etree-components poslems))
			     (cdr lemmas) tac mode))))

