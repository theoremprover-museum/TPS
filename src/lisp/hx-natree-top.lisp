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

(deffile hx-natree-top
  (extension lisp)
  (part-of etr-nat)
  (mhelp "Functions for translating from natural deduction
proofs to expansion proofs."))

(defvar current-natree nil)

(defstruct (natree (:print-function print-natree))
  (name nil :type symbol)
  (number 0 :type integer)
  (free-vars nil :type list)
  (dup-info-pos nil)
  (dup-info-neg nil)
  (justification nil :type symbol)
  (jterm nil)
  (assertion nil)
  (hypo nil :type list)
  (support nil :type list)
  (conclusion nil :type list)
  (discharge nil)
  (heterolist nil :type list)
  (homolist nil :type list)
  (selflist nil :type list)
  (annotation nil :type symbol) ; extraction or normal
  (etree nil))

(defun print-natree (x y z)
  (declare (ignore z)) 
  (let ((*standard-output* y))
    (declare (special *standard-output*))
    (princ (natree-name x))))

(defmexpr pntr
  (print-command t)
  (mainfns print-natree-main)
  (mhelp "Print out the current natree stored in CURRENT-NATREE. Mainly for
the purpose of debugging."))

(defun print-natree-main ()
  (declare (special natree-debug))
  (if CURRENT-NATREE
      (labels ((print-natree-rec (natree) 
		 (let ((support (natree-support natree)))
		   (msg T (natree-name natree) 3)
		   (when natree-debug
		     (msgf (natree-number natree) ": "((natree-assertion natree) . gwff) t))
		   (mapc #'(lambda (hyp) (msg (natree-number hyp) 2)) (natree-hypo natree)) ; cebrown 3/4/00
		   (msg "! " (natree-number natree) ";" 2)
		   (msg "* " (natree-justification natree) " * " (natree-annotation natree) " * ")
		   (cond ((null support) (msg "Leaf Node"))
			 (T (mapc #'(lambda (sup) (msg (natree-number sup) 2)) support)
			    (mapc #'print-natree-rec support))))))
	(print-natree-rec CURRENT-NATREE))
    (msg "Empty natural deduction tree.")))

(defun proof-to-natree (proofname)
  (if (proof-plans proofname) (throwfail "A complete proof with no plan lines is needed."))
  (let* ((lines (proof-lines proofname))
	 (nodes (mapcar #'(lambda (line)
			    (let ((just (line-justification line)))
			      (make-natree :name 
					   (intern (create-namestring-with-hyphen proofname) 
						   (find-package "CL-USER"))
					   :number (line-linenumber line)
					   :justification 
					   (intern (string-upcase (first just))
						   (find-package "AUTO"))
					   :jterm (second just)
					   :assertion (line-assertion line)
					   :hypo (line-hypotheses line)
					   :support (third just))))
			   lines)))
    (dolist (node nodes (car (last nodes)))
       (setf (natree-support node)
	     (mapcar #'(lambda (line) (elt nodes (position line lines))) 
		     (natree-support node)))
       (setf (natree-hypo node)
	     (mapcar #'(lambda (line) (elt nodes (position line lines)))
		     (natree-hypo node))))))

(defun hx-nat-etree (proofname)
  (declare (special newpf))
					; cebrown 8/22/99, copied the following code from auto-suggest to remove subst= lines
  (reconsider proofname)
  (let ((subst=-lines (remove-if-not #'(lambda (x) (member (car (line-justification x)) 
							   '("Subst=" "Sub=" "Assert SYM=" "Sym=")
							   :test 'string=))
				     (get dproof 'lines))))
    (when subst=-lines
      (let ((newpfname (let ((pfname- (intern-str (format nil "~d-" proofname))))
			 (do ((pfname1 (intern-str (create-namestring pfname-)) (intern-str (create-namestring pfname-))))
			     ((not (proof-lines pfname1)) pfname1)))))
	(create-subproof (mapcar #'(lambda (x)
				     (cons x x))
				 (list (linealias (car (proof-lines dproof)))
				       (linealias (car (last (proof-lines dproof))))))
			 newpf))
      (msgf "Some lines in this proof are justified by SUBST= or SYM=" t 
	    "NAT-ETREE will not work properly until they are removed." t t)
      (remove-subst=-lines subst=-lines)))
  (setq current-natree (proof-to-natree dproof))
  (natree-to-etree)
  (let* ((*ignore-statuses* T))
    (mate-prop-msearch)
    (natree-precleanup))
  (setq current-eproof (make-eproof :skolem-method skolem-default)
	master-eproof current-eproof)
  (setf (get (eproof-name current-eproof) 'EPROOF) current-eproof) ; 9/13/01
  (push (eproof-name current-eproof) *eproof-list*) ; 9/13/01
	    #+comment
	    (setf (eproof-etree current-eproof)
		  (make-leaf :shallow (natree-assertion current-natree)
			     :positive nil
			     :parent nil))
	    (setf (etree-leaf current-topnode) T ;;;tell merge-tree-real that this etree does need substitutions.
		  (eproof-etree current-eproof) current-topnode)
	    (update-statuses current-topnode)
	    (init-symmetry (eproof-etree current-eproof) current-eproof)
	    (initialize-mating-search)
    (msgf "If you want to translate the expansion proof back to a natural deduction proof,
you must first merge the etree.  If you want to use the expansion proof to determine
flag settings for automatic search, you should not merge the etree.")
    (if (query "Merge The Etree?" t)
	(progn
	(let ((*ignore-statuses* nil))
	  (update-statuses (eproof-etree current-eproof)))
	(setf (eproof-jform current-eproof)
	      (etree-to-jform (eproof-etree current-eproof)))
	(setf (eproof-leaf-list current-eproof)
	      (find-etree-nodes #'leaf-p (eproof-etree current-eproof)))
	(mate-prop-msearch)
	(let ((*ignore-statuses* nil))
	  (merge-tree))
	(msgf "Proof has been translated -- use ETREE-NAT to reverse the
process." t))
      (msgf "You can use MAKE-EPROOF-CANONICAL to convert the expansion tree to look like a search tree,
then use BUILD-CANONICAL-MATING to transfer the mating to a search tree."))
    (unless (eq proofname dproof)
      (msgf "The current natural deduction proof " dproof " is a modified version
of the original natural deduction proof.

Use RECONSIDER " proofname " to return to the original proof.")))

