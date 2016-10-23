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
;;; File: FTREES  - cebrown - 10/4/00
;;; Definition and functions for "ftrees" -- a different representation 
;;; for expansion trees which are designed for functional programming:
;;; . operations are not destructive
;;; . the information carried at each node is minimal

(deffile ftrees
    (part-of EXPANSION-TREE)
  (extension clisp)
  (mhelp "Functional version of expansion trees."))

(context expansion-trees)

					; The purpose of ftrees is to provide support for functional programming
					; with expansion trees.  The problems with using expansion trees as they
					; are already defined in TPS are:
					; 1. many operations are destructive (deepening, substitution, etc.)
					; 2. many operations assume we are working with the etree associated with the current-eproof
					; 3. etrees carry too much information, which causes problems when trying to manipulate them
					;    while maintaining this information properly
					; 4. have parent nodes which make the references circular

					; Ftrees should overcome these problems as follows:
					; 1. no operations on ftrees should be destructive
					; 2. information at each node should be minimal (name, shallow, exp terms/sel vars, children)
					;    (no parents because we may want to use the same etree in different places)

					; Other invariants:
					; 1. names for annotations on ftrees should be distinct

					; The idea is to do the first part of the search (including set var instantiations and
					; initial mating search) on ftrees, then do the final mating search on an ordinary
					; expansion tree/jform.  So, we need to be able to translate from ftrees to etrees.
					; also, as we may start with an expansion tree, we need to be able to translate from
					; etrees to ftrees.

(defstruct (ftree 
	    (:print-function print-ftree))
  (name 'a :type symbol)
  (shallow nil)
  (positive t)
  (kind nil)				; CON, DIS, IMP, TRUE, FALSE, NEG, REW, EXP, SEL, LEAF
  (rew-just nil :type symbol)
  (exp-terms nil :type list)		; list of expansion terms, usually just vars
  (exp-subs nil :type list)		; assoc of vars with subst terms, subst evars x occur as (x . t), free evars y occur as (y . y)
  (sel-var nil)
  (components nil :type list))

(defvar ftree-verbose nil)

(defun print-ftree (d s k)
  (if ftree-verbose
      (progn
  (format s "~S ~d:~S~%"
	  (ftree-name d)
	  (if (ftree-positive d) "+" "-")
	  (ftree-kind d))
  (pwff (ftree-shallow d))
  (when (ftree-components d)
    (format s "~%   ->")
    (dolist (z (ftree-components d))
      (format s " ~S" (ftree-name z))))
  (format s "~%")
  (cond ((eq (ftree-kind d) 'EXP)
	 (format s "   EXP-TERMS: ~S~%" (ftree-exp-terms d))
	 (format s "   EXP-SUBS: ~S~%" (ftree-exp-subs d)))
	((eq (ftree-kind d) 'SEL)
	 (format s "   SEL-VAR: ~S~%" (ftree-sel-var d)))
	((eq (ftree-kind d) 'REW)
	 (format s "   REW JUST: ~S~%" (ftree-rew-just d))))
  (dolist (z (ftree-components d))
    (print-ftree z s k)))
  (format s "~S " (ftree-name d))))



(defun etree-to-ftree (etree &optional evars)
  (typecase
      etree
    (edisjunction (make-ftree :name (etree-name etree) :kind 'DIS
			      :positive (etree-positive etree)
			      :shallow (strip-to-vars (get-shallow etree))
			      :components (mapcar #'(lambda (e)
						      (etree-to-ftree e evars))
						  (etree-components etree))))
    (implication (make-ftree :name (etree-name etree) :kind 'IMP
			      :positive (etree-positive etree)
			      :shallow (strip-to-vars (get-shallow etree))
			      :components (mapcar #'(lambda (e)
						      (etree-to-ftree e evars))
						  (etree-components etree))))
    (econjunction (make-ftree :name (etree-name etree) :kind 'CON
			      :positive (etree-positive etree)
			      :shallow (strip-to-vars (get-shallow etree))
			      :components (mapcar #'(lambda (e)
						      (etree-to-ftree e evars))
						  (etree-components etree))))
    (rewrite (make-ftree :name (etree-name etree) :kind 'REW :rew-just (rewrite-justification etree)
			 :positive (etree-positive etree)
			 :shallow (strip-to-vars (get-shallow etree))
			 :components (list (etree-to-ftree (car (etree-components etree)) evars))))
    (leaf
     (make-ftree :name (etree-name etree) :kind 'LEAF
		 :positive (etree-positive etree)
		 :shallow (strip-to-vars (get-shallow etree))))
    (empty-dup-info
     (make-ftree :name (etree-name etree) :kind 'LEAF
		 :positive (etree-positive etree)
		 :shallow (strip-to-vars (get-shallow etree))))
    (negation (make-ftree :name (etree-name etree) :kind 'NEG
			  :positive (etree-positive etree)
			  :shallow (strip-to-vars (get-shallow etree))
			  :components (list (etree-to-ftree (car (etree-components etree)) evars))))
    (true (make-ftree :name (etree-name etree) :kind 'TRUE
		      :positive (etree-positive etree)
		      :shallow 'TRUTH))
    (false (make-ftree :name (etree-name etree) :kind 'FALSE
		       :positive (etree-positive etree)
		       :shallow 'FALSEHOOD))
    (selection
     (let* ((s (car (selection-terms etree)))
	    (svar (strip-to-vars s)))
       (make-ftree :name (etree-name etree) :kind 'SEL
		   :positive (etree-positive etree)
		   :shallow (strip-to-vars (get-shallow etree))
		   :sel-var svar
		   :components (list (etree-to-ftree (car (etree-components etree)) evars)))))
    (skolem
     (let* ((s (car (skolem-terms etree)))
	    (svar (strip-to-vars s)))
       (make-ftree :name (etree-name etree) :kind 'SEL
		   :positive (etree-positive etree)
		   :shallow (strip-to-vars (get-shallow etree))
		   :sel-var svar
		   :components (list (etree-to-ftree (car (etree-components etree)) evars)))))
    (expansion 
     (let* ((eterms (expansion-terms etree))
	    (node (make-ftree :name (etree-name etree) :kind 'EXP
			      :positive (etree-positive etree)
			      :shallow (strip-to-vars (get-shallow etree))
			      :exp-terms (mapcar #'strip-to-vars eterms)))
	    (free-evars nil)
	    (subs nil))
       (declare (special subs))
       (dolist (eterm eterms)
	 (when (exp-var-p eterm)
	   (push (cons (exp-var-var eterm) (strip-to-vars (exp-var-subst eterm))) subs)
	   (other-evar-subs (exp-var-subst eterm)))) ; note that the subst of eterm may depend on other evar's, so these need to precede
					; it in the subs (see other-evar-subs)
       (setf (ftree-exp-subs node) subs)
       (dolist (sub subs)
	 (when (eq (car sub) (cdr sub))	; free evar
	   (push (car sub) free-evars)))
       (setf (ftree-components node) (mapcar #'(lambda (e trm)
						 (etree-to-ftree 
						  e 
						  (remove-duplicates
						   (append evars (intersection (free-vars-of trm) free-evars)))))
					     (etree-components etree) 
					     (ftree-exp-terms node)))
       node))
    (t (throwfail "cannot translate node " etree " into an ftree"))))

; addes subst's to subs
(defun other-evar-subs (trm)
  (declare (special subs))
  (if (consp trm)
      (progn
	(other-evar-subs (car trm))
	(other-evar-subs (cdr trm)))
    (if (exp-var-p trm)
	(unless (assoc (exp-var-var trm) subs)
	  (push (cons (exp-var-var trm) (strip-to-vars (exp-var-subst trm)))
		subs)
	  ; next, add other-evar-subs for this var, so they will precede it on the list
	  (other-evar-subs (exp-var-subst trm)))
      nil)))

(defun ftree-junctive (ftree)
  (if (ftree-positive ftree)
      (cond ((member (ftree-kind ftree) '(FALSE DIS IMP)) 'DIS)
	    ((member (ftree-kind ftree) '(TRUE CON EXP)) 'CON)
	    (t 'neutral))
    (cond ((member (ftree-kind ftree) '(FALSE DIS IMP EXP)) 'CON)
	  ((member (ftree-kind ftree) '(TRUE CON)) 'DIS)
	  (t 'neutral))))

(defun ftree-to-etree (ftree)
  (setq *leibniz-var-list* nil)
  (setq *instantiated-eqs-list* nil)
  (setq *instantiated-defs-list* nil)
  (setq *hacked-rewrites-list* nil)
  (setq *banned-conns-list* nil)
  (setq *ho-banned-conns-list* nil)
  (setq *unsubst-exp-vars* nil)
  (setq *rew-unsubst-exps* nil)
  (let ((subst nil))
    (declare (special subst))
    (setq current-eproof
      (make-eproof			; see diy-real in diy.lisp
       :free-vars-in-etree nil
       :substitution-list nil
       :leaf-list nil
       :skolem-constants (mapcar #'(lambda (x)
				     (cons (cdr x) 0))
				 subst)
       :skolem-method skolem-default
       :skolem-node-list nil))
    (setq master-eproof current-eproof)
    (setf (get (eproof-name current-eproof) 'eproof) current-eproof)
    (push (eproof-name current-eproof) *eproof-list*)
    (setf (eproof-symmetry current-eproof) (make-hash-table :test #'eq)) ; init-symmetry, etrees-wffops.lisp
    (let ((etree (ftree-to-etree-rec ftree nil nil)))
      (setq first-order-mode-ms		; see diy-real in diy.lisp
	(first-order-problem-p
	 (mapcar #'(lambda (x) (cons (exp-var-var (car x)) (cdr x)))
		 (eproof-free-vars-in-etree current-eproof))))
      (update-statuses etree)
      (setf (eproof-etree current-eproof) etree)
      (setq current-topnode etree)
      (setq active-mating nil)		; see diy-real in diy.lisp
      (initialize-mating-search)
      (relabel-etree etree)
					; an attempt to handle symmetries, call init-symmetry on each node
					; as far as I can tell, "symmetries" are not used anywhere in the code
					; and I do not know the intended purpose.  I couldn't find anything in the
					; programmer's guide either.  - cebrown 10/5/00
      (find-etree-nodes #'(lambda (x) (init-symmetry x current-eproof) nil))
      (setq *leibniz-var-list* (leibniz-quant *leibniz-var-list*))
;      (fiddle-with-def-leaves current-topnode) ; if dual instantiation is used, skip this - it's incompatible with EQUIVWFFS trick in merge's
      (setf (eproof-leaf-list current-eproof)
	(find-etree-nodes #'leaf-p* current-topnode))
      (setf (eproof-skolem-node-list current-eproof)
	(find-etree-nodes #'skolem-p current-topnode))
      etree)))

(defun ftree-to-etree-rec (ftree &optional parent free-vars)
  (declare (special subst))
 (case (ftree-kind ftree)
    (DIS 
     (let ((node (make-edisjunction
		  :name (ftree-name ftree)
		  :positive (ftree-positive ftree)
		  :junctive (ftree-junctive ftree)
		  :free-vars free-vars
		  :parent parent)))
       (setf (etree-components node)
	 (mapcar #'(lambda (x)
		     (ftree-to-etree-rec x node free-vars))
		 (ftree-components ftree)))
       node))
    (CON
     (let ((node (make-econjunction
		  :name (ftree-name ftree)
		  :positive (ftree-positive ftree)
		  :junctive (ftree-junctive ftree)
		  :free-vars free-vars
		  :parent parent)))
       (setf (etree-components node)
	 (mapcar #'(lambda (x)
		     (ftree-to-etree-rec x node free-vars))
		 (ftree-components ftree)))
       node))
    (IMP
     (let ((node (make-implication
		  :name (ftree-name ftree)
		  :positive (ftree-positive ftree)
		  :junctive (ftree-junctive ftree)
		  :free-vars free-vars
		  :parent parent)))
       (setf (etree-components node)
	 (mapcar #'(lambda (x)
		     (ftree-to-etree-rec x node free-vars))
		 (ftree-components ftree)))
       node))
    (REW
     (let ((node (make-rewrite
		  :name (ftree-name ftree)
		  :positive (ftree-positive ftree)
		  :shallow (simul-substitute-l-term-var subst (ftree-shallow ftree))
		  :junctive 'neutral
		  :justification (ftree-rew-just ftree)
		  :free-vars free-vars
		  :parent parent)))
       (when (eq (ftree-rew-just ftree) 'LEIBNIZ=)
	 (push node *leibniz-var-list*))
       (setf (etree-components node)
	 (list (ftree-to-etree-rec (car (ftree-components ftree)) node free-vars)))
       (when (and (eq (ftree-rew-just ftree) 'EQUIVWFFS)
		  (= (length (ftree-components ftree)) 1)
		  (member (ftree-kind (car (ftree-components ftree))) '(CON DIS))
		  (wffeq (cdar (ftree-shallow (car (ftree-components ftree)))) (ftree-shallow ftree))) ; dual instantiation
	 (let ((sym (gensym)))
	   (push (cons sym (ftree-shallow ftree)) *instantiated-defs-list*)
	   (push (cons node (cons sym (car (etree-components node)))) *hacked-rewrites-list*)))
       node))
    (LEAF
     (make-leaf :positive (ftree-positive ftree)
		:name (ftree-name ftree)
		:shallow (simul-substitute-l-term-var subst (ftree-shallow ftree))
		:free-vars free-vars
		:junctive nil
		:parent parent))
    (EXP
     (let* ((eterms (ftree-exp-terms ftree))
	    (esubs (ftree-exp-subs ftree))
	    (sh (simul-substitute-l-term-var subst (ftree-shallow ftree)))
	    (node (make-expansion
		   :name (ftree-name ftree)
		   :positive (ftree-positive ftree)
		   :shallow sh
		   :junctive 'con
		   :free-vars free-vars
		   :parent parent)))
       (dolist (pair esubs)		; esubs are ordered so that later vars depend on earlier ones (see etree-to-ftree)
	 (unless (assoc (car pair) subst)
	   (push (cons (car pair) (make-exp-var :var (car pair)
						:subst (simul-substitute-l-term-var subst (cdr pair)))) ; selecteds get filled in when relabel-etree/fill-selected is called
		 subst)))
       (setq eterms
	 (mapcar #'(lambda (eterm)
		     (simul-substitute-l-term-var subst eterm))
		 eterms))
       (setf (expansion-terms node) eterms)
       (when eterms
	 (dolist (eterm eterms)
;	   (msgf "node = " node)
;	   (msgf "eterm = " eterm)
	   (if (exp-var-p eterm)
	       (if (eq (exp-var-var eterm) (exp-var-subst eterm))
		   (progn
;		     (msgf "free")
		     (push (cons eterm node) (eproof-free-vars-in-etree current-eproof))
		     (push (list eterm) (expansion-prim-vars node)))
		 (progn
;		   (msgf "subst w/ free ")
		   (push eterm (eproof-substitution-list current-eproof))
		   (dolist (new-var (substitutable-vars-of (exp-var-subst eterm))) ; see substitute-in-etree in etrees-wffops2.lisp
;		     (msgf "   var : " new-var)
		     (pushnew (cons new-var node) (eproof-free-vars-in-etree current-eproof))
		     (pushnew (list new-var) (expansion-prim-vars node)
			      :key #'car))))
	     (dolist (new-var (substitutable-vars-of eterm)) ; see substitute-in-etree in etrees-wffops2.lisp
	       (pushnew (cons new-var node) (eproof-free-vars-in-etree current-eproof))
	       (pushnew (list new-var) (expansion-prim-vars node)
			:key #'car))))
	 (setf (etree-components node)
	   (mapcar #'(lambda (x y)
		       (ftree-to-etree-rec x node
					   (union (exp-vars-in y) free-vars)))
		   (ftree-components ftree)
		   eterms))
	 (dolist (kid (etree-components node))
	   (add-to-syms kid (car (etree-components node)))))
       node))
    (SEL
     (let* ((sv (ftree-sel-var ftree))
	    (term (make-skolem-term :term sv :parameter sv))
	    (sh (simul-substitute-l-term-var subst (ftree-shallow ftree)))
	    (node (make-selection
		   :name (ftree-name ftree)
		   :positive (ftree-positive ftree)
		   :junctive 'neutral
		   :shallow sh
		   :free-vars free-vars
		   :terms (list term)
		   :parent parent)))
       (when (skolem-term-p term)
	 (setq subst (acons sv term subst)))
       (setf (etree-components node)
	 (list (ftree-to-etree-rec (car (ftree-components ftree)) node free-vars)))
       (when (skolem-term-p term)
	 (push (cons term 0)
	       (eproof-skolem-constants current-eproof)))
       node))
    (NEG
     (let ((node (make-negation 
		  :name (ftree-name ftree)
		  :positive (ftree-positive ftree)
		  :junctive 'neutral
		  :free-vars free-vars
		  :parent parent)))
       (setf (etree-components node)
	 (list (ftree-to-etree-rec (car (ftree-components ftree)) node free-vars)))
       node))
    (TRUE
     (make-true
      :name (ftree-name ftree)
      :positive (ftree-positive ftree)
      :junctive (ftree-junctive ftree)
      :free-vars free-vars
      :parent parent))
    (FALSE
     (make-false
      :name (ftree-name ftree)
      :positive (ftree-positive ftree)
      :junctive (ftree-junctive ftree)
      :free-vars free-vars
      :parent parent))
    (t (throwfail "Cannot translate ftree " (ftree-name ftree) " to expansion tree"))))

(defmateop save-etree
    (mate-alias save-etree)
  (mate-result-> ignore))

(defmateop restore-etree
    (mate-alias restore-etree)
  (mate-result-> ignore))

(defwffop save-etree
    (argtypes filespec)
  (argnames savefile)
  (arghelp "File in which to save the etree")
  (defaultfns (lambda (savefile)
		(list (if (eq savefile '$)
			  (namestring
			   (make-pathname% :name (string-downcase
						  (string dproof)) 
					   :type "etr"
					   :version :newest))
			savefile))))
  (mainfns save-etree)
  (mhelp "Converts the current etree to an internal representation and saves this to a file.
This currently only works for etrees generated with SKOLEM-DEFAULT nil.

Example of how to use SAVE-ETREE for X2106 and later use RESTORE-ETREE:
<3>MATE x2106
<Mate4>GO
<Mate5>MERGE-TREE
<Mate6>SAVE-ETREE
SAVEFILE (FILESPEC): File in which to save the etree [\"x2106.etr\"]>
Later come back into TPS and do the following:
<0>MATE x2108 (or MATE whatever)
<Mate1>RESTORE-ETREE
LOADFILE (FILESPEC): File in which to load the etree [\"x2108.etr\"]>\"x2106.etr\"
<Mate2>GO
<Mate3>LEAVE
Merge the expansion tree? [Yes]>Y
Now ETREE-NAT should work.

"))


(defwffop restore-etree
    (argtypes filespec)
  (argnames loadfile)
  (arghelp "File in which to load the etree")
  (defaultfns (lambda (loadfile)
		(list (if (eq loadfile '$)
			  (namestring
			   (make-pathname% :name (string-downcase
						  (string dproof)) 
					   :type "etr"
					   :version :newest))
			loadfile))))
  (mainfns restore-etree)
  (mhelp "Loads an etree and makes this the current etree.

Example of how to use SAVE-ETREE for X2106 and later use RESTORE-ETREE:
<3>MATE x2106
<Mate4>GO
<Mate5>MERGE-TREE
<Mate6>SAVE-ETREE
SAVEFILE (FILESPEC): File in which to save the etree [\"x2106.etr\"]>
Later come back into TPS and do the following:
<0>MATE x2108 (or MATE whatever)
<Mate1>RESTORE-ETREE
LOADFILE (FILESPEC): File in which to load the etree [\"x2108.etr\"]>\"x2106.etr\"
<Mate2>GO
<Mate3>LEAVE
Merge the expansion tree? [Yes]>Y
Now ETREE-NAT should work.

"))

(defun save-etree (savefile)
  (when (pathnamep savefile) (setq savefile (namestring savefile)))
  (setq savefile
    (merge-pathnames savefile
		     (make-pathname% :name (string dproof)
				     :type "etr"
				     :version :newest)))
  (let ((ftree (etree-to-ftree current-topnode)))
    (with-open-file (*standard-output* savefile
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
      (write-ftree ftree)))
  (format t "File ~A written." (namestring savefile)))

(defun write-ftree (ftree)
  (format t "(~S ~S ~S ~S~%~S~%~S~%~S~%~S~%" (ftree-name ftree) 
	  (ftree-positive ftree)
	  (ftree-kind ftree)
	  (ftree-rew-just ftree)
	  (ftree-shallow ftree)
	  (ftree-exp-terms ftree)
	  (ftree-exp-subs ftree)
	  (ftree-sel-var ftree))
  (format t "~%(")
  (dolist (k (ftree-components ftree))
    (write-ftree k))
  (format t "))~%"))

(defun restore-etree (loadfile)
  (restore-etree-real loadfile (cons nil source-path)))

(defun restore-etree-real (loadfile source-path)
  (setq loadfile
    (merge-pathnames loadfile
		     (make-pathname% :directory (car source-path)
				     :name (string dproof)
				     :type "etr"
				     :version :newest)))
  (if (probe-file loadfile)
      (with-open-file (in loadfile :direction :input :if-does-not-exist :error)
	(let ((finfo (read in)))
	  (ftree-to-etree (finfo-to-ftree finfo))))
    (if (cdr source-path)
	(restore-etree-real loadfile (cdr source-path))
      (throwfail "File " loadfile " does not exist; check SOURCE-PATH."))))

(defun finfo-to-ftree (finfo)
  (unless (and (listp finfo)
	       (= (length finfo) 9))
    (throwfail "File does not contain an etree."))
  (make-ftree
   :name (car finfo)
   :positive (nth 1 finfo)
   :kind (nth 2 finfo)
   :rew-just (nth 3 finfo)
   :shallow (nth 4 finfo)
   :exp-terms (nth 5 finfo)
   :exp-subs (nth 6 finfo)
   :sel-var (nth 7 finfo)
   :components (mapcar #'finfo-to-ftree (nth 8 finfo))))

(defun strip-to-vars (gwff)
  "Replaces exp-vars by their var slots and skolem-terms by their parameter slots"
  (cond ((exp-var-p gwff) (exp-var-var gwff))
	((skolem-term-p gwff) (skolem-term-parameter gwff))
	((lsymbol-q gwff) gwff)
	((boundwff-q gwff)
	 (cons (car gwff) (strip-to-vars (cdr gwff))))
	(t (cons (strip-to-vars (car gwff))
		 (strip-to-vars (cdr gwff))))))

(defun exp-vars-in (wff)
  (if (consp wff)
      (union (exp-vars-in (car wff))
	     (exp-vars-in (cdr wff)))
    (if (exp-var-p wff)
	(cons wff (exp-vars-in (exp-var-subst wff)))
      nil)))

(defun dual-rew-ftree-node-p (ftree)
  (if (and (eq (ftree-kind ftree) 'rew)
	   (eq (ftree-rew-just ftree) 'EQUIVWFFS))
      (let ((kid (car (ftree-components ftree))))
	(and (member (ftree-kind ftree) '(CON DIS))
	     (wffeq (cdar (ftree-shallow kid)) (ftree-shallow ftree))))
    nil))

(defun deepen-ftree-to-literals (ftree)
  (cond ((eq (ftree-kind ftree) 'leaf)
	 (deepen-ftree-leaf ftree))
	((dual-rew-ftree-node-p ftree) ; don't want to deepen the left leaf coming from a dual instantiation
	 (let* ((kid (car (ftree-components ftree)))
		(kid1 (car (ftree-components kid)))
		(kid2 (cadr (ftree-components kid)))
		(kid3 (deepen-ftree-to-literals kid2)))
	   (if (eq kid2 kid3)
	       ftree
	     (let* ((new (copy-ftree ftree))
		    (new2 (copy-ftree kid)))
	       (setf (ftree-components new) (list new2))
	       (setf (ftree-components new2) (list kid1 kid3))
	       new))))
	(t
	 (let* ((kids (ftree-components ftree))
		(kids2 (mapcar #'deepen-ftree-to-literals kids)))
	   (if (equal kids kids2)
	       ftree
	     (let ((new (copy-ftree ftree)))
	       (setf (ftree-components new) kids2)
	       new))))))

(defun deepen-ftree-leaf (ftree)
  (let ((new (gwff-to-ftree (ftree-shallow ftree) (ftree-positive ftree))))
    (if (leaf-p new)
	ftree
      new)))

					; note that I am ignoring truthvalues-hack and add-truth
					; (gwff-to-etree uses these, but they should only be used at the top level,
					; and I intend for gwff-to-ftree to be used to build ftree's that will be
					; included in other ftree's -- as it is used in deepen-ftree-leaf above).
(defun gwff-to-ftree (wff &optional (pos t) evars)
  (let* ((defns (get-all-defns wff))
	 (rewrite-defns (fix-rewrite-defns defns)))
    (declare (special defns rewrite-defns))
    (gwff-to-ftree-rec wff pos evars)))

					; this code is similar to deepen-leaf-node-real in etrees-wffops.lisp
(defun gwff-to-ftree-rec (wff &optional (pos t) evars)
  (declare (special defns rewrite-defns lambda-conv))
  (let ((bwff (lambda-norm wff))
	ewff)
    (if (not (wffeq wff bwff))
	(case lambda-conv
	  ((beta-only beta-eta-separate)
	   (make-ftree :kind 'REW :name (intern-str (create-namestring rewrite-name))
		       :rew-just 'BETA :positive pos :shallow wff
		       :components (list (gwff-to-ftree-rec bwff pos evars))))
	  (beta-eta-together
	   (make-ftree :kind 'REW :name (intern-str (create-namestring rewrite-name))
		       :rew-just 'LAMBDA :positive pos :shallow wff
		       :components (list (gwff-to-ftree-rec (lnorm-eta bwff) pos evars))))
	  (t (throwfail "Unknown value " lambda-conv " for flag LAMBDA-CONV")))
      (if (and (neq lambda-conv 'beta-only)
	       (progn
		 (setq ewff (lnorm-eta wff))
		 (not (wffeq wff ewff))))
	  (make-ftree :kind 'REW :rew-just 'ETA :positive pos :shallow wff
		      :name (intern-str (create-namestring rewrite-name))
		      :components (list (gwff-to-ftree-rec ewff pos evars)))
	(cond ((and-p wff)
	       (make-ftree :kind 'CON :positive pos :shallow wff :name (intern-str (create-namestring econj-name))
			   :components (list (gwff-to-ftree-rec (cdar wff) pos evars)
					     (gwff-to-ftree-rec (cdr wff) pos evars))))
	      ((or-p wff)
	       (make-ftree :kind 'DIS :positive pos :shallow wff :name (intern-str (create-namestring edisj-name))
			   :components (list (gwff-to-ftree-rec (cdar wff) pos evars)
					     (gwff-to-ftree-rec (cdr wff) pos evars))))
	      ((implies-p wff)
	       (make-ftree :kind 'IMP :positive pos :shallow wff :name (intern-str (create-namestring imp-name))
			   :components (list (gwff-to-ftree-rec (cdar wff) (not pos) evars)
					     (gwff-to-ftree-rec (cdr wff) pos evars))))
	      ((wffeq wff 'TRUTH)
	       (make-ftree :kind 'TRUE :positive pos :shallow wff :name (intern-str (create-namestring true-name))))
	      ((wffeq wff 'FALSEHOOD)
	       (make-ftree :kind 'FALSE :positive pos :shallow wff :name (intern-str (create-namestring false-name))))
	      ((not-p wff)
	       (make-ftree :kind 'NEG :positive pos :shallow wff :name (intern-str (create-namestring neg-name))
			   :components (list (gwff-to-ftree-rec (cdr wff) (not pos) evars))))
	      ((equiv-p wff)
	       (let ((newwff (rewrite-equiv wff pos)))
		 (make-ftree :kind 'REW :positive pos :shallow wff :name (intern-str (create-namestring rewrite-name))
			     :rew-just (if (and-p newwff) 'equiv-implics 'equiv-disjs)
			     :components (list (gwff-to-ftree-rec newwff pos evars)))))
	      ((and min-quantifier-scope
		    (or (a-bd-wff-p wff) (e-bd-wff-p wff))
		    (mqs-applicable wff))
	       (make-ftree :kind 'REW :positive pos :shallow wff :name (intern-str (create-namestring rewrite-name))
			   :rew-just 'RULEQ
			   :components (list (gwff-to-ftree-rec (min-quant-scope wff) pos evars))))
	      ((if pos (a-bd-wff-p wff) (e-bd-wff-p wff))
	       (let ((evar (funcall ren-var-fn (caar wff))))
		 (make-ftree :kind 'EXP :positive pos :shallow wff :name (intern-str (create-namestring expansion-name))
			     :exp-terms (list evar)
			     :exp-subs (list (cons evar evar))
			     :components (list (gwff-to-ftree-rec 
						(substitute-term-var
						 evar (caar wff) (cdr wff))
						pos (cons evar evars))))))
	      ((if pos (e-bd-wff-p wff) (a-bd-wff-p wff))
	       (let* ((svar (funcall ren-var-fn (caar wff))))
		 (make-ftree :kind 'SEL :positive pos :shallow wff :name (intern-str (create-namestring selection-name))
			     :sel-var svar
			     :components (list (gwff-to-ftree-rec 
						(substitute-term-var svar (caar wff) (cdr wff))
						pos evars)))))
	      ((and (equals-p wff)
		    (member rewrite-equalities '(lazy2 dual)))
	       (let ((newwff (expand-top= wff pos)))
		 (if (wffeq wff newwff)
		     (make-ftree :kind 'LEAF :positive pos :shallow wff ; equality is not to be expanded at all
				 :name (intern-str (create-namestring leaf-name)))
		   (make-ftree :kind 'REW :rew-just 'EQUIVWFFS :positive pos :shallow wff
			       :name (intern-str (create-namestring rewrite-name))
			       :components 
			       (list (make-ftree :kind (if pos 'CON 'DIS) :positive pos :shallow (acons (if pos 'AND 'OR) wff newwff)
						 :components (list
							      (make-ftree :kind 'LEAF :positive pos :shallow wff
									  :name (intern-str (create-namestring leaf-name)))
							      (gwff-to-ftree-rec newwff pos evars))))))))
	      ((and (neq rewrite-equalities 'none)
		    (equals-p wff)
		    (if (eq rewrite-equalities 'auto::only-ext)
			(contains-ext= (caar wff))
		      t))
	       (multiple-value-bind
		   (newwff just) (expand-top= wff pos)
		 (when just
		   (make-ftree :kind 'REW :rew-just just :positive pos :shallow wff
			       :name (intern-str (create-namestring rewrite-name))
			       :components (list (gwff-to-ftree-rec newwff pos evars))))))
	      ;;***  this is complicated; I mostly just copied it from deepen-leaf-node-real in etrees-wffops.lisp - cebrown - 10/5/00
					;  note that this assumes rewrite-defns is not the usual flag value,
					;  but the result of applying fix-rewrite-defns to the list of defns occurring in the wff, i.e.,
					;  an alist of ([LAZY1|LAZY2|...] . <list of defns>)
	      ((and (or (cdr (assoc 'eager rewrite-defns))
			(cdr (assoc 'lazy1 rewrite-defns))
			(cdr (assoc 'dual rewrite-defns))
			(cdr (assoc 'lazy2 rewrite-defns)))
		    (let ((rewrite-defns (setdiff (append (cdr (assoc 'eager rewrite-defns)) ; and here rewrite-defns is temporarily
							  (cdr (assoc 'dual rewrite-defns)) ; set to be a list of defns
							  (cdr (assoc 'lazy1 rewrite-defns)) ; so that contains-some-defn will work
							  (cdr (assoc 'lazy2 rewrite-defns)))
						  '(equiv))))
		      (contains-some-defn wff)))
	     (let ((firstgwff wff)
		   (gwff wff))
	       (when (cdr (assoc 'eager rewrite-defns))
		     (let ((rewrite-defns (setdiff (cdr (assoc 'eager rewrite-defns)) '(equiv))))
		       (setq gwff
			     (do ((gwff (instantiate-all gwff (setdiff defns rewrite-defns)) ; send it a list of exceptions
					(instantiate-all gwff (setdiff defns rewrite-defns)))
				  (oldgwff nil gwff))
				 ((wffeq oldgwff gwff) gwff)))))
	       (when (and (wffeq gwff firstgwff) (cdr (assoc 'lazy1 rewrite-defns)))
		     (let ((rewrite-defns (setdiff (cdr (assoc 'lazy1 rewrite-defns)) '(equiv))))
		       (setq gwff (instantiate-1-from-list gwff rewrite-defns))))
	       (if (and (wffeq gwff firstgwff) (or (cdr (assoc 'lazy2 rewrite-defns)) ; here I diverge a bit from deepen-leaf-node-real
						   (cdr (assoc 'dual rewrite-defns)))) ; - cebrown - 10/5/00
		   (progn
		     (let ((rewrite-defns (setdiff (or (cdr (assoc 'lazy2 rewrite-defns))
						       (cdr (assoc 'dual rewrite-defns)))
						   '(equiv))))
		       (setq gwff (instantiate-1-from-list gwff rewrite-defns)))
		     (if (wffeq gwff firstgwff) ; really shouldn't happen, or something is wrong with contains-some-defn called above
			 (make-ftree :kind 'LEAF :positive pos :shallow wff
				     :name (intern-str (create-namestring leaf-name)))
		       (make-ftree :kind 'REW :rew-just 'EQUIVWFFS ; dual instantiation
				   :positive pos :shallow wff :name (intern-str (create-namestring rewrite-name))
				   :components
				   (list (make-ftree :kind (if pos 'CON 'DIS) :positive pos :shallow (acons (if pos 'AND 'OR) wff gwff)
						     :components
						     (list (make-ftree :kind 'LEAF :positive pos :shallow wff)
							   (gwff-to-ftree-rec gwff pos evars)))))))
		 (if (wffeq gwff firstgwff) ; again, really shouldn't happen, or something is wrong with contains-some-defn called above
		     (make-ftree :kind 'LEAF :positive pos :shallow wff
				 :name (intern-str (create-namestring leaf-name)))
		   (make-ftree :kind 'REW :rew-just 'EQUIVWFFS :positive pos :shallow wff
			       :name (intern-str (create-namestring rewrite-name))
			       :components (list (gwff-to-ftree-rec gwff pos evars)))))))
	    (t (make-ftree :kind 'LEAF :positive pos :shallow wff
			   :name (intern-str (create-namestring leaf-name)))))))))

(defun gwff-to-ftree-dup (wff pos numofdups)
  (if (or (and (e-bd-wff-p wff) (not pos))
	  (and (a-bd-wff-p wff) pos))
      (let ((kidsl nil)
	    (expvl nil)
	    (z (bindvar wff)))
	(do ((z2 (fresh-var (type z) (getnameroot z))
		 (fresh-var (type z) (getnameroot z)))
	     (i 0 (1+ i)))
	    ((> i numofdups))
	    (push z2 expvl) 
	    (push (gwff-to-ftree (substitute-l-term-var z2 z (cdr wff)) pos)
		  kidsl))
	(let ((exp (make-ftree-exp wff expvl kidsl)))
	  (setf (ftree-exp-subs exp)
		(append (mapcar #'(lambda (x)
				    (cons x x))
				expvl)
			(ftree-exp-subs exp)))
	  exp))
    (gwff-to-ftree wff pos)))

(defun make-ftree-conjunction (fl)
  (cond ((null fl) nil)
	((null (cdr fl)) (car fl))
	(t
	 (make-ftree-con (car fl) (make-ftree-conjunction (cdr fl))))))
	   
(defun gwff-to-rulep-ftree (wff &optional (pos nil))
  (cond ((implies-p wff)
	 (make-ftree-imp (gwff-to-rulep-ftree (cdar wff) (not pos))
			 (gwff-to-rulep-ftree (cdr wff) pos)))
	((or-p wff)
	 (make-ftree-dis (gwff-to-rulep-ftree (cdar wff) pos)
			 (gwff-to-rulep-ftree (cdr wff) pos)))
	((and-p wff)
	 (make-ftree-con (gwff-to-rulep-ftree (cdar wff) pos)
			 (gwff-to-rulep-ftree (cdr wff) pos)))
	((not-p wff)
	 (make-ftree-neg (gwff-to-rulep-ftree (cdr wff) (not pos))))
	((equiv-p wff)
	 (let ((a (cdar wff))
	       (b (cdr wff)))
	   (make-ftree-rew wff
			   'EQUIV-IMPLICS
			   (gwff-to-rulep-ftree
			    (acons 'AND
				   (acons 'IMPLIES a b)
				   (acons 'IMPLIES b a))
			    pos))))
	((eq wff 'TRUTH) (make-ftree-true pos))
	((eq wff 'FALSEHOOD) (make-ftree-false pos))
	(t (make-ftree-leaf wff pos))))

; should I delete this?
					; a simple recursive algorithm is not enough to do merging
					; correctly and effeciently
					; see Pfenning's Thesis, p. 45-48

(defun merge-ftrees (f1 f2)
  (let* ((m1 (list 'MERGE (list f1 f2)))
	 (merge-result nil))
    (declare (special merge-result))
    (dolist (k (append (ftree-components f1) (ftree-components f2)))
	    (setq merge-result (append (ftree-assoc k k) merge-result)))
    (setq merge-result (acons f1 m1 (acons f2 m1 merge-result)))
    (merge-ftree-real)
    (cdr (assoc f1 merge-result))))

(defun merge-ftree-real ()
  (declare (special merge-result))
  (do ((redex (merge-ftree-find-redex)
	      (merge-ftree-find-redex)))
      ((null redex))
      (merge-ftree-contract redex)
      (merge-reconstruct-ftree-all)))

(defun merge-ftree-contract (redex)
  (declare (special merge-result))
  (if redex
      (if (eq (car redex) 'EXP-MERGE)
	  (let* ((f (cdr redex))
		 (trms (ftree-exp-terms-s f))
		 (kids (ftree-components f))
		 (tmassoc nil)
		 (sh (ftree-shallow f))
		 (x (bindvar sh)))
	    (mapc #'(lambda (trm kid)
		      (let ((a (assoc trm tmassoc :test #'wffeq)))
			(if a
			    (setq tmassoc (substitute (cons trm
							    (list 'MERGE
								  (list kid (cdr a))
								  (substitute-l-term-var
								   trm x (cdr sh))))
						      a tmassoc))
			  (setq tmassoc (cons (cons trm kid) tmassoc)))))
		  trms kids)
	    (merge-ftree-replace-all
	     (list (cons f
			 (list 'EXP (mapcar #'cdr tmassoc) sh (mapcar #'car tmassoc))))))
	(if (eq (car redex) 'MERGE)
	    (let ((f1 (caadr redex))
		  (f2 (cadadr redex)))
	      (cond ((or (eq (ftree-kind f1) 'LEAF) (eq f1 f2))
		     (merge-ftree-replace-all (acons redex f2 nil)))
		    ((eq (ftree-kind f2) 'LEAF)
		     (merge-ftree-replace-all (acons redex f1 nil)))
		    ((and (eq (ftree-kind f1) 'REW) (eq (ftree-kind f2) 'REW))
		     (let ((sh1 (ftree-shallow (car (ftree-components f1))))
			   (sh2 (ftree-shallow (car (ftree-components f2)))))
		       (if (wffeq sh1 sh2)
			   (let* ((m1 (list 'MERGE (list (car (ftree-components f1))
							 (car (ftree-components f2)))))
				  (mf (list 'REW (list m1) 
					    (ftree-shallow f1)
					    (ftree-rew-just f1))))
			     (merge-ftree-replace-all 
			      (acons (car (ftree-components f1)) m1
				     (acons (car (ftree-components f2)) m1
					    (acons redex mf nil)))))
			 (let ((mf (use-equivwffs-to-merge-ftrees f1 f2)))
			   (merge-ftree-replace-all (acons redex mf nil))))))
		    ((or (eq (ftree-kind f1) 'REW) (eq (ftree-kind f2) 'REW))
		     (let ((mf (use-equivwffs-to-merge-ftrees f1 f2)))
		       (merge-ftree-replace-all (acons redex mf nil))))
		    ((eq (ftree-kind f1) (ftree-kind f2))
		     (case (ftree-kind f1)
			   ((CON DIS IMP)
			    (let* ((m1 (list 'MERGE
					     (list (car (ftree-components f1))
						   (car (ftree-components f2)))))
				   (m2 (list 'MERGE
					     (list (cadr (ftree-components f1))
						   (cadr (ftree-components f2)))))
				   (mf (list (ftree-kind f1) (list m1 m2))))
			      (merge-ftree-replace-all
			       (acons (car (ftree-components f1)) m1
				      (acons (car (ftree-components f2)) m1
					     (acons (cadr (ftree-components f1)) m2
						    (acons (cadr (ftree-components f2)) m2
							   (acons redex mf nil))))))))
			   (NEG
			    (let* ((m1 (list 'MERGE
					     (list (car (ftree-components f1))
						   (car (ftree-components f2)))))
				   (mf (list 'NEG (list m1))))
			      (merge-ftree-replace-all 
			       (acons (car (ftree-components f1)) m1
				      (acons (car (ftree-components f2)) m1
					     (acons redex mf nil))))))
			   ((TRUE FALSE)
			    (merge-ftree-replace-all (acons redex f1 nil)))
			   (EXP
			    (let ((mf (list 'EXP (append (ftree-components f1)
							 (ftree-components f2))
					    (ftree-shallow f1)
					    (append (ftree-exp-terms-s f1)
						    (ftree-exp-terms-s f2)))))
			      (merge-ftree-replace-all (acons redex mf nil))))
			   (SEL
			    (let* ((u (ftree-sel-var f1))
				   (v (ftree-sel-var f2))
				   (w (fresh-var (type u) (getnameroot u)))
				   (theta (list (cons u w) (cons v w)))
				   (f3 (rename-selected-vars-of-ftree-rec
					theta (car (ftree-components f1))))
				   (f4 (rename-selected-vars-of-ftree-rec
					theta (car (ftree-components f2))))
				   (m1 (list 'MERGE (list f3 f4)))
				   (mf (list 'SEL (list m1) (ftree-shallow f1) w))
				   (pairs (append 
					   (ftree-assoc (car (ftree-components f1)) f3)
					   (ftree-assoc (car (ftree-components f2)) f4))))
			      (merge-ftree-replace-sel-all 
			       (acons redex mf pairs) theta)))))
		    (t ; last ditch effort - just use an equivwffs rewrite
		     (merge-ftree-replace-all
		      (acons redex
			     (use-equivwffs-to-merge-ftrees f1 f2) nil)))))))
    nil))

(defun merge-ftree-find-redex ()
  (declare (special merge-result))
  (merge-ftree-find-redex-1 (mapcar #'cdr merge-result)))
  
(defun merge-ftree-find-redex-0 (mftree)
  (if (ftree-p mftree)
      (if (eq (ftree-kind mftree) 'EXP)
	  (let ((trms (ftree-exp-terms-s mftree)))
	    (if (equal (remove-duplicates trms :test #'wffeq) trms)
		(merge-ftree-find-redex-1 (ftree-components mftree))
	      (cons 'EXP-MERGE mftree)))
	(merge-ftree-find-redex-1 (ftree-components mftree)))
    (if (and (eq (car mftree) 'MERGE)
	     (ftree-p (caadr mftree))
	     (ftree-p (cadadr mftree)))
	mftree
      (merge-ftree-find-redex-1 (cadr mftree)))))

(defun merge-ftree-find-redex-1 (mftrees)
  (if mftrees
      (or (merge-ftree-find-redex-0 (car mftrees))
	  (merge-ftree-find-redex-1 (cdr mftrees)))
    nil))

(defun merge-ftree-replace-all (pairs)
  (declare (special merge-result))
  (let ((new-pairs pairs)
;	(count 1)
	)
    (declare (special new-pairs))
    (dolist (a merge-result)
;	    (print count)
;	    (incf count)
	    (rplacd a (merge-ftree-replace (cdr a))))))

(defun merge-ftree-replace-sel-all (pairs theta)
  (declare (special merge-result))
  (let ((new-pairs pairs))
    (declare (special new-pairs))
    (dolist (a merge-result)
	    (rplacd a (merge-ftree-replace-sel theta (cdr a))))))

; an ftree may become an mftree because of replacing an exp node
; with an exp mf with merges underneath it.
(defun merge-ftree-replace (mftree)
  (declare (special new-pairs))
  (let ((z (assoc mftree new-pairs :test #'equal)))
    (if z
	(cdr z)
      (if (ftree-p mftree)
	  (let ((kids (mapcar #'(lambda (x)
				  (merge-ftree-replace x))
			      (ftree-components mftree))))
	    (if (equal kids (ftree-components mftree))
		(progn
		  (push (cons mftree mftree) new-pairs)
		  mftree)
	      (let ((newmf (case (ftree-kind mftree)
				 ((NEG CON DIS IMP) (list (ftree-kind mftree) kids))
				 (REW (list 'REW kids (ftree-shallow mftree)
					    (ftree-rew-just mftree)))
				 (EXP (list 'EXP kids (ftree-shallow mftree)
					    (ftree-exp-terms-s mftree)))
				 (SEL (list 'SEL kids (ftree-shallow mftree)
					    (ftree-sel-var mftree))))))
		(push (cons mftree newmf) new-pairs)
		newmf)))
	(let ((ret (cons (car mftree)
			 (cons (mapcar #'(lambda (x)
					   (merge-ftree-replace x))
				       (cadr mftree))
			       (cddr mftree)))))
	  (push (cons mftree ret) new-pairs)
	  ret)))))

; returns an ftree when given an ftree
(defun merge-ftree-replace-sel (theta mftree)
  (declare (special new-pairs))
  (let ((z (assoc mftree new-pairs :test #'equal)))
    (if z
	(cdr z)
      (if (ftree-p mftree)
	  (let* ((kids2 (mapcar #'(lambda (kid)
				    (merge-ftree-replace-sel theta kid))
				(ftree-components mftree)))
		 (sh2 (simul-substitute-l-term-var theta (ftree-shallow mftree)))
		 (newf
		  (cond ((eq (ftree-kind mftree) 'SEL)
			 (let ((a (assoc (ftree-sel-var mftree) theta)))
			   (if a
			       (make-ftree-sel sh2 (cdr a) (car kids2))
			     (make-ftree-sel sh2 (ftree-sel-var mftree) (car kids2) mftree))))
			((eq (ftree-kind mftree) 'EXP)
			 (let ((trms2 (mapcar #'(lambda (x)
						  (simul-substitute-l-term-var theta x))
					      (ftree-exp-terms-s mftree))))
			   (make-ftree-exp sh2 trms2 kids2 mftree)))
			(t
			 (if (and (wffeq sh2 (ftree-shallow mftree))
				  (equal kids2 (ftree-components mftree)))
			     mftree
			   (let ((f2 (copy-ftree mftree)))
			     (setf (ftree-shallow f2) sh2)
			     (setf (ftree-components f2) kids2)
			     f2))))))
	    (push (cons mftree newf) new-pairs)
	    newf)
	(let* ((kids (mapcar #'(lambda (x)
				 (merge-ftree-replace-sel theta x))
			     (cadr mftree)))
	       (newmf (case (car mftree)
			    (REW (list 'REW kids (simul-substitute-l-term-var theta (caddr mftree))
				       (cadddr mftree)))
			    (EXP (list 'EXP kids (simul-substitute-l-term-var theta (caddr mftree))
				       (mapcar #'(lambda (x)
						   (simul-substitute-l-term-var theta x))
					       (cadddr mftree))))
			    (SEL (list 'SEL kids (simul-substitute-l-term-var theta (caddr mftree))
				       (cadddr mftree)))
			    (t (list (car mftree) kids)))))
	  (push (cons mftree newmf) new-pairs)
	  newmf)))))

(defun merge-reconstruct-ftree-all ()
  (declare (special merge-result))
  (let ((new-pairs nil))
    (declare (special new-pairs))
    (dolist (a merge-result)
	    (rplacd a (merge-reconstruct-ftree (cdr a))))))

(defun merge-reconstruct-ftree (mftree)
  (declare (special new-pairs))
  (if (ftree-p mftree)
      mftree ; no point in saving this on new-pairs - requires no computation
    (let ((z (assoc mftree new-pairs :test #'equal)))
      (if z
	  (cdr z)
	(let ((kids (mapcar #'merge-reconstruct-ftree (cadr mftree))))
	  (if (find-if-not #'(lambda (f) (ftree-p f)) kids)
	      ; still has merge/exp-merge nodes, can't make ftree yet
	      (let ((newmf (cons (car mftree) (cons kids (cddr mftree)))))
		(push (cons mftree newmf) new-pairs)
		newmf)
	    ; can make ftree, unless merge/exp-merge node
	    (let ((newf (case (car mftree)
			      (CON (make-ftree-con (car kids) (cadr kids)))
			      (DIS (make-ftree-dis (car kids) (cadr kids)))
			      (IMP (make-ftree-imp (car kids) (cadr kids)))
			      (NEG (make-ftree-neg (car kids)))
			      (REW (make-ftree-rew (caddr mftree) (cadddr mftree)
						   (car kids)))
			      (EXP (make-ftree-exp (caddr mftree) (cadddr mftree)
						   kids))
			      (SEL (make-ftree-sel (caddr mftree) (cadddr mftree)
						   (car kids)))
			      (MERGE (cons 'MERGE (cons kids (cddr mftree))))
			      (t (throwfail "Unexpected mftree case: " mftree)))))
	      (push (cons mftree newf) new-pairs)
	      newf)))))))
	       
(defun use-equivwffs-to-merge-ftrees (f1 f2)
  (make-ftree-rew 
   (ftree-shallow f1)
   'EQUIVWFFS
   (if (ftree-positive f1)
       (make-ftree-con f1 f2)
     (make-ftree-dis f1 f2))))

(defun rename-selected-vars-of-ftree (f)
  (let* ((svars (selected-vars-of-ftree f))
	 (theta (mapcar #'(lambda (u)
			    (cons u (fresh-var (type u) (getnameroot u))))
			svars)))
    (rename-selected-vars-of-ftree-rec theta f)))

(defun rename-selected-vars-of-ftree-rec (theta ftree)
  (let ((kids2 (mapcar #'(lambda (kid)
			   (rename-selected-vars-of-ftree-rec theta kid))
		       (ftree-components ftree)))
	(sh2 (simul-substitute-l-term-var theta (ftree-shallow ftree))))
    (cond ((eq (ftree-kind ftree) 'SEL)
	   (let ((a (assoc (ftree-sel-var ftree) theta)))
	     (if a
		 (make-ftree-sel sh2 (cdr a) (car kids2))
	       (make-ftree-sel sh2 (ftree-sel-var ftree) (car kids2) ftree))))
	  ((eq (ftree-kind ftree) 'EXP)
	   (let ((trms2 (mapcar #'(lambda (x)
				    (simul-substitute-l-term-var theta x))
				(ftree-exp-terms-s ftree))))
	     (make-ftree-exp sh2 trms2 kids2 ftree)))
	  (t
	   (if (and (wffeq sh2 (ftree-shallow ftree))
		    (equal kids2 (ftree-components ftree)))
	       ftree
	     (let ((f2 (copy-ftree ftree)))
	       (setf (ftree-shallow f2) sh2)
	       (setf (ftree-components f2) kids2)
	       f2))))))


					; some constructor functions - if the new ftree node would be the same as the old one
					; (if an oldf was given), then oldf is returned
(defun make-ftree-leaf (wff pos &optional oldf)
;  (unless (equal (type wff) 'O)
;    (break))
  (if (and (ftree-p oldf)
	   (eq (ftree-kind oldf) 'LEAF)
	   (eq (ftree-positive oldf) pos))
      oldf
    (make-ftree :kind 'LEAF :positive pos :shallow wff
		:name (intern-str (create-namestring leaf-name)))))

(defun make-ftree-exp (wff terms kids &optional oldf)
;  (unless (equal (type wff) 'O)
;    (break))
  (if (and (ftree-p oldf)
	   (wffeq-ab wff (ftree-shallow oldf))
	   (not (member nil
			(mapcar #'(lambda (x y) (wffeq-ab x y))
				terms (ftree-exp-terms-s oldf))))
	   (equal kids (ftree-components oldf)))
      oldf
  (let ((f (make-ftree :kind 'EXP :name (intern-str (create-namestring expansion-name))
		       :shallow wff
		       :positive (a-bd-wff-p wff) :components kids)))
    (dolist (trm (reverse terms) f)
      (let ((evar (funcall ren-var-fn (caar wff))))
	(push evar (ftree-exp-terms f))
	(push (cons evar trm) (ftree-exp-subs f)))))))

(defun make-ftree-sel (wff svar kid &optional oldf)
;  (unless (equal (type wff) 'O)
;    (break))
  (if (and (ftree-p oldf)
	   (eq (ftree-kind oldf) 'SEL)
	   (wffeq (ftree-shallow oldf) wff)
	   (eq (ftree-sel-var oldf) svar)
	   (equal (ftree-components oldf) (list kid)))
      oldf
    (make-ftree :kind 'SEL :name (intern-str (create-namestring selection-name))
		:shallow wff
		:positive (e-bd-wff-p wff) :components (list kid)
		:sel-var svar)))

(defun make-ftree-rew (wff just kid &optional oldf)
  (if (and (ftree-p oldf)
	   (eq (ftree-kind oldf) 'REW)
	   (wffeq (ftree-shallow oldf) wff)
	   (eq (ftree-rew-just oldf) just)
	   (equal (ftree-components oldf) (list kid)))
      oldf
    (make-ftree :kind 'REW :name (intern-str (create-namestring rewrite-name))
		:positive (ftree-positive kid) :shallow wff
		:rew-just just
		:components (list kid))))

(defun make-ftree-con (kid1 kid2 &optional oldf)
  (if (and (ftree-p oldf)
	   (eq (ftree-kind oldf) 'CON)
	   (equal (ftree-components oldf) (list kid1 kid2)))
      oldf
    (make-ftree :kind 'CON :name (intern-str (create-namestring econj-name))
		:shallow (acons 'AND (ftree-shallow kid1) (ftree-shallow kid2))
		:positive (ftree-positive kid1)
		:components (list kid1 kid2))))

(defun make-ftree-dis (kid1 kid2 &optional oldf)
  (if (and (ftree-p oldf)
	   (eq (ftree-kind oldf) 'DIS)
	   (equal (ftree-components oldf) (list kid1 kid2)))
      oldf
  (make-ftree :kind 'DIS :name (intern-str (create-namestring edisj-name))
	      :shallow (acons 'OR (ftree-shallow kid1) (ftree-shallow kid2))
	      :positive (ftree-positive kid1)
	      :components (list kid1 kid2))))

(defun make-ftree-imp (kid1 kid2 &optional oldf)
  (if (and (ftree-p oldf)
	   (eq (ftree-kind oldf) 'IMP)
	   (equal (ftree-components oldf) (list kid1 kid2)))
      oldf
  (make-ftree :kind 'IMP :name (intern-str (create-namestring imp-name))
	      :shallow (acons 'IMPLIES (ftree-shallow kid1) (ftree-shallow kid2))
	      :positive (ftree-positive kid2)
	      :components (list kid1 kid2))))

(defvar *global-pos-true-ftree* 
  (make-ftree :kind 'TRUE :name 'TRUE1
	      :positive t :shallow 'TRUTH))

(defvar *global-neg-true-ftree* 
  (make-ftree :kind 'TRUE :name 'TRUE2
	      :positive nil :shallow 'TRUTH))

(defvar *global-pos-false-ftree* 
  (make-ftree :kind 'FALSE :name 'FALSE1
	      :positive t :shallow 'FALSEHOOD))

(defvar *global-neg-false-ftree* 
  (make-ftree :kind 'FALSE :name 'FALSE2
	      :positive nil :shallow 'FALSEHOOD))

(defun make-ftree-true (pos)
  (if pos
      *global-pos-true-ftree*
    *global-neg-true-ftree*))

(defun make-ftree-false (pos)
  (if pos
      *global-pos-false-ftree*
    *global-neg-false-ftree*))

(defun make-ftree-neg (kid &optional oldf)
  (if (and (ftree-p oldf)
	   (eq (ftree-kind oldf) 'IMP)
	   (equal (ftree-components oldf) (list kid)))
      oldf
  (make-ftree :kind 'NEG :name (intern-str (create-namestring neg-name))
	      :shallow (cons 'NOT (ftree-shallow kid))
	      :positive (not (ftree-positive kid))
	      :components (list kid))))

; currently unused
(defun make-ftree-rew-chain (f sh)
  (let* ((fsh (ftree-shallow f))
	 (c (common-defn-eq-refl=-lam-reduct fsh sh)))
    (if c
	(let ((rews1 (reverse (car c)))
	      (rews2 (cdr c)))
	  (if (cdr rews2)
	      (throwfail "Merge Failed Due to Lack of Reduct")
	    (do ((rews3 (cdr rews1) (cddr rews3))
		 (f2 f (make-ftree-rew (cadr rews3)
				       (case (caar rews3)
					     (LEIBNIZ 'LEIBNIZ=)
					     (EXT 'EXT=)
					     (DEFN 'DEFN)
					     (BINDER 'BINDER-DEFN)
					     (REFL 'REFL=)
					     (t (caadr rews2)))
				       f2)))
		((null rews3) f2))))
      (throwfail "Merge Failed Due to Lack of Reduct"))))

(defun make-fntype (argtps rettp)
  (if argtps
      (cons (make-fntype (cdr argtps) rettp) (car argtps))
    rettp))

(defun ftree-free-evars-in-image (vl subs)
  (if vl
      (let ((a (assoc (car vl) subs)))
	(if (eq (car vl) (cdr a))
	    (adjoin (car vl) (ftree-free-evars-in-image (cdr vl) subs))
	  (ftree-free-evars-in-image (union (free-vars-of (cdr a)) (cdr vl)) subs)))
    nil))

(defun ftree-parameters (ftree)
  (union (free-vars-of (ftree-shallow ftree))
	 (selected-vars-of-ftree ftree)))

(defun selected-vars-of-ftree (ftree)
  (if (eq (ftree-kind ftree) 'SEL)
      (cons (ftree-sel-var ftree)
	    (selected-vars-of-ftree (car (ftree-components ftree))))
    (apply #'nconc
	   (mapcar 
	    #'selected-vars-of-ftree
	    (ftree-components ftree)))))

; makes exp vars with :selected slot filled in, 2/12/01- modified 5/5/01 to get rid of skolems in ftrees
(defun substitutable-exp-vars-of-ftree (ftree)
  (mapcar #'(lambda (x)
	      (make-exp-var :var (car x) :subst (car x)
			    :selected (cdr x)))
	  (ftree-all-banned ftree)))

(defun substitutable-vars-of-ftree (ftree)
  (remove-duplicates
   (substitutable-vars-of-ftree-rec ftree)))

(defun substitutable-vars-of-ftree-rec (ftree)
  (let ((evs (apply #'nconc
		    (mapcar 
		     #'substitutable-vars-of-ftree-rec
		     (ftree-components ftree)))))
    (when (eq (ftree-kind ftree) 'EXP)
      (dolist (pair (ftree-exp-subs ftree))
	(when (eq (car pair) (cdr pair))
	  (push (car pair) evs))))
    evs))

(defun substitutions-in-ftree (ftree)
  (let ((top-evars nil))
    (declare (special top-evars))
    (remove-if-not
     #'(lambda (x) (member (car x) top-evars))
     (substitutions-in-ftree-rec ftree nil))))

(defun substitutions-in-ftree-rec (ftree theta)
  (declare (special top-evars))
  (dolist (k (ftree-components ftree))
    (setq theta (substitutions-in-ftree-rec k theta)))
  (when (eq (ftree-kind ftree) 'EXP)
    (setq top-evars (remove-if-not #'symbolp (append (ftree-exp-terms ftree) top-evars)))
    (dolist (pair (ftree-exp-subs ftree))
      (unless (assoc (car pair) theta)
	(push (cons (car pair) (lnorm (simul-substitute-l-term-var theta (cdr pair)))) ; lambda normalize for readability
	      theta))))
  theta)

					; this is for substituting for a symbol that stands for an evar
					; the substitution affects the exp-subs
					; of appropriate expansion nodes, and actually does the substitution
					; *and* deepens
(defun substitute-in-ftree (trm var ftree &optional (deepen t))
  (let ((params (union (free-vars-of (ftree-shallow ftree))
		       (selected-vars-of-ftree ftree))))
    (declare (special params))
    (let ((fsub (substitute-in-ftree-rec trm var ftree)))
      (if deepen
	  (deepen-ftree-to-literals fsub)
	fsub))))

(defun substitute-in-ftree-rec (trm var ftree)
  (declare (special params))
  (let ((retval ftree))
    (when (eq (ftree-kind ftree) 'EXP)
	(let* ((subs (ftree-exp-subs ftree))
	       (a (assoc var subs)))
	  (if a
	      (if (eq (car a) (cdr a))
		  (let ((new-evars (setdiff
				    (setdiff (free-vars-of trm)
					     params)
				    (mapcar #'car subs))))
		    (setq retval (copy-ftree ftree))
		    (setf (ftree-exp-subs retval)
		      (append (mapcar #'(lambda (x) (cons x x)) new-evars)
			      (acons (car a) trm (remove a subs))))) ; important that it's put at the beginning!
		(unless (wffeq-ab (cdr a) trm)
		  (throwfail "var " var " has already been given value " (cdr a) " in " ftree)))
	    (when (member var subs :test #'(lambda (v sub)
					     (free-in v (cdr sub))))
	      (setq retval (copy-ftree ftree))
	      (push (cons var trm) (ftree-exp-subs retval))))))
    (when (free-in var (ftree-shallow ftree))
      (when (eq ftree retval)
	(setq retval (copy-ftree ftree)))
      (setf (ftree-shallow retval) (substitute-term-var trm var (ftree-shallow retval)))
      (when (eq (ftree-kind ftree) 'SEL) ; need to change evar deps if old skolem term depended on the old evar
	(let ((sh (ftree-shallow retval)))
	  (when (member (ftree-sel-var retval) (free-vars-of sh)) ; we've created a cycle
	    (throw 'cycle (ftree-sel-var retval)))))) ;  "Substitution in Ftree led to a cycle with selected var "
    (let* ((kids (ftree-components ftree))
	   (kids2 (mapcar #'(lambda (k)
			      (substitute-in-ftree-rec trm var k))
			  kids)))
      (unless (equal kids kids2)
	(when (eq ftree retval)
	  (setq retval (copy-ftree ftree)))
	(setf (ftree-components retval) kids2)
	retval))
;    (when (eq retval ftree)
;      (msg "did not change " (ftree-name ftree) t))
    retval))

; this is for subtituting evars - not for params
; rename-selected-vars-of-ftree-rec is for param substs
(defun simul-substitute-in-ftree (theta ftree &optional (deepen t))
  (let ((params (union (free-vars-of (ftree-shallow ftree))
		       (selected-vars-of-ftree ftree))))
    (declare (special params))
    (let ((f (simul-substitute-in-ftree-rec theta ftree)))
      (if deepen
	  (deepen-ftree-to-literals f)
	f))))

; this is for subtituting evars - not for params
; rename-selected-vars-of-ftree-rec is for param substs
(defun simul-substitute-in-ftree-rec (theta ftree)
  (declare (special params))
  (let* ((retval ftree)
	 (sh (ftree-shallow ftree))
	 (shsub (simul-substitute-l-term-var theta sh)))
    (when (eq (ftree-kind ftree) 'EXP)
      (let* ((subs (ftree-exp-subs ftree))
	     (changed nil)
	     (new-evars nil)
	     (newsubs nil)
	     (oldsubs nil))
	(dolist (pair subs)
	  (let ((a (assoc (car pair) theta)))
	    (if a
		(if (eq (car pair) (cdr pair))
		    (let ((these-new-evars
			   (setdiff
			    (setdiff (free-vars-of (cdr a))
				     params)
			    (mapcar #'car subs))))
		      (setq new-evars
			(append new-evars these-new-evars))
		      (setq changed t)
		      (push a newsubs)
		      (setq newsubs (append (mapcar #'(lambda (x) (cons x x))
						    these-new-evars)
					    newsubs)))
		  (if (wffeq-ab (cdr a) (cdr pair))
		      (progn
			(when (eq (car pair) (cdr pair))
			  (push (car pair) new-evars))
			(push pair newsubs))
		    (throwfail "var " (car pair) " has already been given value " ((cdr pair) . gwff) " in " ftree)))
	      (progn
		(when (eq (car pair) (cdr pair))
		  (push (car pair) new-evars))
		(push pair oldsubs)))))
	(when (or changed (not (wffeq-ab sh shsub)))
	  (setq retval (copy-ftree ftree))
	  (setf (ftree-exp-subs retval) (append newsubs oldsubs)))))
    (unless (wffeq-ab sh shsub)
      (when (eq retval ftree)
	(setq retval (copy-ftree ftree)))
      (setf (ftree-shallow retval) shsub))
    (let ((kids2 
	   (mapcar #'(lambda (f)
		       (simul-substitute-in-ftree-rec theta f))
		   (ftree-components ftree))))
      (when (eq (ftree-kind ftree) 'SEL)
	(when (member (ftree-sel-var retval) (free-vars-of sh)) ; we've created a cycle
	  (throw 'cycle (ftree-sel-var retval))))
      (unless (equal (ftree-components ftree) kids2)
	(when (eq retval ftree)
	  (setq retval (copy-ftree ftree)))
	(setf (ftree-components retval) kids2))
      retval)))

(defun ftree-to-jform (f)
  (declare (special dissolve))
  (let ((j (normalize-jform (ftree-to-jform-rec f))))
    (dolist (conn dissolve j)
      (setq j (normalize-jform (dissolve-jform conn j))))))

(defun ftree-lambda-rewrite-p (f)
  (and (ftree-p f)
       (eq (ftree-kind f) 'REW)
       (member (ftree-rew-just f) '(LAMBDA BETA ETA))))
  
(defun allow-ftree-node-in-jform (f)
  (declare (special dissolve allow-nonleaf-conns))
  (or (eq (ftree-kind f) 'LEAF)
      (member (ftree-name f) ALLOW-NONLEAF-CONNS)
      (and (not (ftree-lambda-rewrite-p f))
	   (not (eq (ftree-kind f) 'NEG)) ; cebrown 3/6/01
	   (member 'ALL ALLOW-NONLEAF-CONNS))
      (and (eq (ftree-kind f) 'REW)
	   (not (ftree-lambda-rewrite-p f))
	   (member 'REWRITES ALLOW-NONLEAF-CONNS))
      (dolist (d dissolve nil)
	(when (or (eq (ftree-name f) (car d))
		  (eq (ftree-name f) (cdr d)))
	  (return t)))))

(defun ftree-to-jform-rec (f)
  (if (eq (ftree-kind f) 'LEAF)
      (make-nnf-literal (ftree-name f) (ftree-shallow f)
			(ftree-positive f) 1)
    (let ((j (case (ftree-junctive f)
		   (CON (make-conjunction 
			 :components (mapcar #'ftree-to-jform-rec (ftree-components f))))
		   (DIS (make-disjunction 
			 :components (mapcar #'ftree-to-jform-rec (ftree-components f))))
		   (t (ftree-to-jform-rec (car (ftree-components f)))))))
      (if (ALLOW-FTREE-NODE-IN-JFORM f)
	  (make-conjunction
	   :components (list (make-nnf-literal 
			      (ftree-name f) (ftree-shallow f)
			      (ftree-positive f) 1)
			     j))
	j))))

(defun ftree-assoc (f-from f-to)
  (if (ftree-lambda-rewrite-p f-from)
      (if (ftree-lambda-rewrite-p f-to)
	  (acons f-from f-to
		 (ftree-assoc (car (ftree-components f-from))
			      (car (ftree-components f-to))))
	(ftree-assoc (car (ftree-components f-from)) f-to))
    (if (ftree-lambda-rewrite-p f-from)
	(ftree-assoc f-from (car (ftree-components f-to)))
      (acons f-from f-to
	     (apply #'append
		    (mapcar #'(lambda (x y)
				(ftree-assoc x y))
			    (ftree-components f-from)
			    (ftree-components f-to)))))))

(defun ftree-mating-image (mating f-from f-to)
  (if mating
      (acons (ftree-image (caar mating) f-from f-to)
	     (ftree-image (cdar mating) f-from f-to)
	     (ftree-mating-image (cdr mating) f-from f-to))
    nil))

(defun ftree-image (name f-from f-to)
  (or (ftree-image-rec name f-from f-to)
      (throwfail "no ftree node " name " in " f-from " corresponding to " f-to)))

(defun ftree-image-rec (name f-from f-to)
  (if (eq (ftree-name f-from) name)
      (if (or (and (eq (ftree-kind f-to) 'REW)
		   (eq (ftree-rew-just f-to) 'LAMBDA))
	      (eq (ftree-kind f-to) 'NEG))
	  (ftree-image-rec name f-from (car (ftree-components f-to)))
	(ftree-name f-to))
    (ftree-list-image-rec name (ftree-components f-from) (ftree-components f-to))))

(defun ftree-list-image-rec (name froml tol)
  (if (and froml tol)
      (or
       (ftree-image-rec name (car froml) (car tol))
       (ftree-list-image-rec name (cdr froml) (cdr tol)))
    nil))

(defun find-ftree-node (name ftree)
  (or (find-ftree-node-rec name ftree)
      (throwfail "Node " name " is not in ftree " ftree)))

(defun find-ftree-node-rec (name ftree)
  (if (eq (ftree-name ftree) name)
      ftree
    (find-ftree-node-list name (ftree-components ftree))))

(defun find-ftree-node-list (name flist)
  (if flist
      (or (find-ftree-node-rec name (car flist))
	  (find-ftree-node-list name (cdr flist)))
    nil))

(defun list-ftree-nodes (f)
  (cons f 
	(apply #'append
	       (mapcar #'list-ftree-nodes (ftree-components f)))))

(defun ftree-instantiated-exp-term (trm subs)
  (dolist (sub (reverse subs) trm)
    (setq trm (substitute-term-var (cdr sub) (car sub) trm))))

					; applies the substs and returns the actual terms, not just the evars
(defun ftree-exp-terms-s (f)
  (mapcar #'(lambda (x)
	      (ftree-instantiated-exp-term x (ftree-exp-subs f)))
	  (ftree-exp-terms f)))

(defun ftree-first-junct (f)
  (if (eq (ftree-kind f) 'REW)
      (ftree-first-junct (car (ftree-components f)))
    (if (member (ftree-kind f) '(CON DIS IMP))
	(car (ftree-components f))
      (throwfail "Expected " f " to be a binary prop op node"))))

(defun ftree-second-junct (f)
  (if (eq (ftree-kind f) 'REW)
      (ftree-first-junct (car (ftree-components f)))
    (if (member (ftree-kind f) '(CON DIS IMP))
	(cadr (ftree-components f))
      (throwfail "Expected " f " to be a binary prop op node"))))

(defun ftree-neg-child (f)
  (if (eq (ftree-kind f) 'REW)
      (ftree-neg-child (car (ftree-components f)))
    (if (eq (ftree-kind f) 'NEG)
	(car (ftree-components f))
      (throwfail "Expected " f " to be a negation node"))))

(defun ftree-positions (f subf &optional pos)
  (if (eq f subf)
      (list pos)
    (let ((posl nil))
      (do ((i 0 (+ i 1))
	   (kids (ftree-components f) (cdr kids)))
	  ((null kids)
	   posl)
	  (setq posl (append (ftree-positions (car kids) subf
					      (append pos (list i)))
			     posl))))))

(defun get-ftree-pos (f pos)
  (if pos
      (get-ftree-pos (nth (car pos) (ftree-components f)) (cdr pos))
    f))

(defun check-acyclicity-ftree (ftree)
  (let* ((all-banned (ftree-all-banned ftree))
	 (all-subs (ftree-all-subs ftree (reduce 'append (mapcar #'cdr all-banned))))
	 (*vars-checked* nil))
    (dolist (sub all-subs t)
      (when (and (not (memq (car sub) *vars-checked*))
		 (find-cycle-beginning-at (list (car sub)) all-banned all-subs))
	(return nil)))))

(defun ftree-all-subs (ftree allb)
  (let ((all-subs nil))
    (when (eq (ftree-kind ftree) 'EXP)
      (dolist (trm (ftree-exp-terms ftree))
	(let ((params (all-occurs-in (ftree-instantiated-exp-term trm (ftree-exp-subs ftree)) allb)))
	  (when params
	    (push (cons trm params) all-subs)))))
    (dolist (kid (ftree-components ftree))
      (setq all-subs
	(append (ftree-all-subs kid allb)
		all-subs)))
    all-subs))

					; returns an alist of (<exp var> . <list of sel vars dominated by exp var>)
(defun ftree-all-banned (ftree)
  (ftree-all-banned-rec ftree))

					; returns values : 1. alist of (<exp var> . <list of sel vars dominated by exp var>)
					;              and 2. sel vars selected beneath this node
(defun ftree-all-banned-rec (ftree)
  (if (eq (ftree-kind ftree) 'EXP)
      (let ((all-banned nil)
	    (sel-vars nil))
	(do ((trms (ftree-exp-terms ftree) (cdr trms))
	     (kids (ftree-components ftree) (cdr kids)))
	    ((not trms) (values all-banned sel-vars))
	  (multiple-value-bind
	      (ab2 sv2)
	      (ftree-all-banned-rec (car kids))
;	    (when sv2 ; otherwise there is nothing new for all-banned
	      (setq all-banned
		(all-banned-acons
		 (car trms) sv2
		 (all-banned-append ab2 all-banned)))
	      (setq sel-vars (append sv2 sel-vars))
	      (let* ((trm2 (ftree-instantiated-exp-term (car trms) (ftree-exp-subs ftree)))
		     (fvlist (free-vars-of trm2)))
		(dolist (v fvlist)
		  (unless (eq v (car trms)) ; not the original exp var
		    (when (assoc v (ftree-exp-subs ftree)) ; some other exp var
		      (setq all-banned (all-banned-acons v sv2 all-banned))))))
;	      )
	  ))
	(values all-banned sel-vars))
    (if (eq (ftree-kind ftree) 'SEL)
	(multiple-value-bind
	    (ab sv)
	    (ftree-all-banned-rec (car (ftree-components ftree)))
	  (values ab (cons (ftree-sel-var ftree) sv)))
      (let ((all-banned nil)
	    (sel-vars nil))
	(dolist (kid (ftree-components ftree))
	  (multiple-value-bind
	      (ab2 sv2)
	      (ftree-all-banned-rec kid)
	    (setq all-banned (all-banned-append ab2 all-banned))
	    (setq sel-vars (append sv2 sel-vars))))
	(values all-banned sel-vars)))))

; returns alist of (<sel var> . <sel vars>)
; where (a . b) is on the list if there is an exp term t with
; a free in t and b is selected below t
(defun ftree-embedding-reln (f)
  (let ((sel-vars (selected-vars-of-ftree f)))
    (mapcar #'(lambda (a)
		(cons a (ftree-embedded-sel-vars a (list f))))
	    sel-vars)))

(defun ftree-embedded-sel-vars (a fl)
  (if fl
      (let ((f (car fl))
	    (retval nil))
	(if (eq (ftree-kind f) 'EXP)
	    (progn
	      (mapc #'(lambda (trm kid)
			(when (free-in a trm)
			  (setq retval (append (selected-vars-of-ftree kid)
					       retval))))
		    (ftree-exp-terms-s f)
		    (ftree-components f))
	      (append retval
		      (ftree-embedded-sel-vars a (cdr fl))))
	  (ftree-embedded-sel-vars a (append (ftree-components f) (cdr fl)))))
    nil))

(defun ftree-embedding-reln-cyclic (r)
  (let ((*vars-checked* nil))
    (declare (special *vars-checked*))
    (dolist (ab r nil)
	    (when (embedding-cycle-starting-at (car ab) (cdr ab) r)
	      (return-from ftree-embedding-reln-cyclic t)))))

(defun embedding-cycle-starting-at (a l r)
  (declare (special *vars-checked*))
  (if (member a *vars-checked*)
      nil
    (dolist (b l nil)
	    (let ((bc (assoc b r)))
	      (if (embedding-cycle-starting-at b (cdr bc) r)
		  (return-from embedding-cycle-starting-at t)
		(push b *vars-checked*))))))

; 
(defun compose-embedding-reln (a bl r)
  (let ((cl (cdr (assoc a r)))
	(retval nil))
    (dolist (cd r retval)
	    (unless (eq (car cd) a)
	      (if (member (car cd) bl)
		  (push (cons (car cd) (append cl (cdr cd)))
			retval)
		(push cd retval))))))

(defun duplicate-evars-max (evars f &optional max)
  (let* ((retval f)
	 (kids (mapcar #'(lambda (x)
			   (duplicate-evars-max evars x max))
		       (ftree-components f))))
    (if (and (eq (ftree-kind f) 'exp)
	     (intersection evars (ftree-exp-terms f))
	     (or (not max) (< (length (ftree-exp-terms f)) max)))
	(progn
	  (setq retval (copy-ftree f))
	  (let ((evar (funcall ren-var-fn (car (intersection evars (ftree-exp-terms f)))))
		(sh (ftree-shallow f)))
	    (setf (ftree-exp-terms retval) (append (ftree-exp-terms retval) (list evar)))
	    (setf (ftree-exp-subs retval) (cons (cons evar evar) (ftree-exp-subs f)))
	    (setf (ftree-components retval)
	      (append kids (list (gwff-to-ftree (substitute-term-var evar (caar sh) (cdr sh))
						(ftree-positive retval)))))))
      (unless (equal kids (ftree-components f))
	(setq retval (copy-ftree f))
	(setf (ftree-components retval) kids)))
    retval))

(defun ftree-deep (f)
  (case (ftree-kind f)
    (DIS (deepify-list (mapcar #'ftree-deep (ftree-components f))
		       'OR))
    (CON (deepify-list (mapcar #'ftree-deep (ftree-components f))
		       'AND))
    (IMP (deepify-list (mapcar #'ftree-deep (ftree-components f))
		       'IMPLIES))
    (REW (ftree-deep (car (ftree-components f))))
    (LEAF (ftree-shallow f))
    (EXP (if (ftree-positive f)
	     (if (ftree-components f)
		 (deepify-list (mapcar #'ftree-deep (ftree-components f))
			       'AND)
	       'TRUTH)
	   (if (ftree-components f)
	       (deepify-list (mapcar #'ftree-deep (ftree-components f))
			     'OR)
	     'FALSEHOOD)))
    (SEL (if (ftree-components f)
	     (ftree-deep (car (ftree-components f)))
	   (ftree-shallow f)))
    (NEG (cons 'NOT (ftree-deep (car (ftree-components f)))))
    (TRUE 'TRUTH)
    (FALSE 'FALSEHOOD)))

; given: a wff, eqn wff, and an ftree node f+-
; returns: an ftree node+- for wff, pos ftree node for eqn, list of conns
(defun make-ftree-subst (wff eqn f)
  (unless (equals-p eqn)
    (throwfail "make-ftree-subst expects " (eqn . gwff) " to be an equation"))
  (let* ((left (cdar eqn))
	 (right (cdr eqn))
	 (x (fresh-var (type left) '\x)))
    (multiple-value-bind
     (abstrm parity)
     (abstract-term-from-equation x left right wff (ftree-shallow f))
     (let ((abstrm2 (acons x 'LAMBDA abstrm)))
       (case parity
	     (EQUAL
	      (values f (make-ftree-leaf eqn t) nil)) ; don't really need the eqn
	     (REV ; wff == [abstrm2 right] & f_wff == [abstrm2 left]
;	      (msgf "wff: " (wff . gwff) t "abstrm2 right: " ((lnorm (cons abstrm2 right)) . gwff) t "f_wff: " ((ftree-shallow f) . gwff) t "abstrm2 left: " ((lnorm (cons abstrm2 left)) . gwff) t) ; delete me
	      (let* ((leaf3 (make-ftree-leaf (lnorm (cons abstrm2 left))
					     (not (ftree-positive f))))
		     (leaf4 (make-ftree-leaf (lnorm (cons abstrm2 right)) (ftree-positive f)))
		     (f3 (if (ftree-positive f) leaf3 (make-ftree-neg leaf3)))
		     (f4 (if (ftree-positive f) leaf4 (make-ftree-neg leaf4)))
		     (e (caar eqn))
		     (leib (acons (instantiate-equality-basic e) left right))
		     (lleib (lnorm leib))
		     (q (bindvar lleib))
		     (abstrm3 (acons x 'lambda 
				     (if (ftree-positive f) abstrm (cons 'NOT abstrm))))
		     (imp (substitute-l-term-var abstrm3 q (cdr lleib)))
		     (eqnf
		      (make-ftree-rew
		       eqn 'LEIBNIZ=
		       (make-ftree-rew
			leib 'LAMBDA
			(make-ftree-exp lleib (list abstrm3)
					(list (make-ftree-rew imp 'LAMBDA
							      (make-ftree-imp f3 f4))))))))
		(values leaf4 eqnf (acons (ftree-name leaf3) (ftree-name f) nil))))
	     (FWD ; wff == [abstrm2 left] & f_wff == [abstrm2 right]
;	      (msgf "wff: " (wff . gwff) t "abstrm2 right: " ((lnorm (cons abstrm2 right)) . gwff) t "f_wff: " ((ftree-shallow f) . gwff) t "abstrm2 left: " ((lnorm (cons abstrm2 left)) . gwff) t) ; delete me
	      (let* ((leaf3 (make-ftree-leaf (lnorm (cons abstrm2 left))
					     (ftree-positive f)))
		     (leaf4 (make-ftree-leaf (lnorm (cons abstrm2 right))
					     (not (ftree-positive f))))
		     (f3 (if (ftree-positive f) (make-ftree-neg leaf3) leaf3))
		     (f4 (if (ftree-positive f) (make-ftree-neg leaf4) leaf4))
		     (e (caar eqn))
		     (leib (acons (instantiate-equality-basic e) left right))
		     (lleib (lnorm leib))
		     (q (bindvar lleib))
		     (abstrm3 (acons x 'lambda 
				     (if (ftree-positive f) (cons 'NOT abstrm) abstrm)))
		     (imp (substitute-l-term-var abstrm3 q (cdr lleib)))
		     (eqnf
		      (make-ftree-rew
		       eqn 'LEIBNIZ=
		       (make-ftree-rew
			leib 'LAMBDA
			(make-ftree-exp lleib (list abstrm3)
					(list (make-ftree-rew imp 'LAMBDA
							      (make-ftree-imp f3 f4))))))))
		(values leaf3 eqnf (acons (ftree-name leaf4) (ftree-name f) nil)))))))))

(defun abstract-term-from-equation (x left right wff1 wff2 &optional varstack)
  (let ((globalp (and (not (intersection (mapcar #'car varstack) (free-vars-of wff1)))
		      (not (intersection (mapcar #'cdr varstack) (free-vars-of wff2))))))
    (cond ((wffeq-ab1 wff1 wff2 varstack)
	   (values wff1 'equal))
	  ((and globalp (wffeq-ab wff1 left) (wffeq-ab wff2 right))
	   (values x 'FWD))
	  ((and globalp (wffeq-ab wff2 left) (wffeq-ab wff1 right))
	   (values x 'REV))
	  ((boundwff-p wff1)
	   (if (and (boundwff-p wff2) (equal (binder wff1) (binder wff2))
		    (equal (unabbreviated-type (bindvar wff1))
			   (unabbreviated-type (bindvar wff2))))
	       (multiple-value-bind
		(abstrm1 parity1)
		(abstract-term-from-equation x left right (cdr wff1) (cdr wff2)
					     (acons (bindvar wff1) (bindvar wff2)
						    varstack))
		(values (acons (bindvar wff1) (binder wff1) abstrm1)
			parity1))
	     (throwfail "wff " (wff1 . gwff) t "and wff " (wff2 . gwff) t
			"are not equal up to " (left . gwff) " = " (right . gwff))))
	  ((consp wff1)
	   (if (consp wff2)
	       (multiple-value-bind
		(abstrm1 parity1)
		(abstract-term-from-equation x left right (car wff1) (car wff2))
		(multiple-value-bind
		 (abstrm2 parity2)
		 (abstract-term-from-equation x left right (cdr wff1) (cdr wff2))
		 (if (eq parity1 'EQUAL)
		     (values (cons abstrm1 abstrm2) parity2)
		   (if (eq parity2 'EQUAL)
		       (values (cons abstrm1 abstrm2) parity1)
		     (if (eq parity1 parity2)
			 (values (cons abstrm1 abstrm2) parity1)
		       (throwfail "Cannot use eqn in both directions simultaneously"))))))
	     (throwfail "wff " (wff1 . gwff) t "and wff " (wff2 . gwff) t
			"are not equal up to " (left . gwff) " = " (right . gwff))))
	  (t
	   (throwfail "wff " (wff1 . gwff) t "and wff " (wff2 . gwff) t
		      "are not equal up to " (left . gwff) " = " (right . gwff))))))
	     
; for debugging
; f : a negative ftree
; clist : alist of symbols naming nodes in f, should be a complete mating
(defun check-ftree-pf (f clist)
  (let ((ftree-clist nil))
    (declare (special ftree-clist))
    (dolist (conn clist)
	    (let ((f1 (find-ftree-node (car conn) f))
		  (f2 (find-ftree-node (cdr conn) f)))
	      (unless (wffeq-ab (ftree-shallow f1) (ftree-shallow f2))
		(msgf f1 ": " ((ftree-shallow f1) . gwff) t)
		(msgf f2 ": " ((ftree-shallow f2) . gwff) t)
		(throwfail "Bad Conn"))
	      (push (cons f1 f2)
		    ftree-clist)))
    (check-ftree f)
    (check-ftree-mating nil (list f))))
			 
; 
; pos : list of positive ftree nodes
; neg : list of negative ftree nodes
; special ftree-clist: alist of mated ftree nodes
(defun check-ftree-mating (pos neg)
  (declare (special ftree-clist))
  (let ((fhyp nil)
	(fconc nil))
    (dolist (g pos)
	    (if fhyp
		(setq fhyp (make-ftree-con g fhyp))
	      (setq fhyp g)))
    (dolist (g neg)
	    (if fconc
		(setq fconc (make-ftree-dis g fconc))
	      (setq fconc g)))
    (let ((node (if fhyp
		    (if fconc
			(make-ftree-imp fhyp fconc)
		      (make-ftree-neg fhyp))
		  (if fconc
		      fconc
		    (throwfail "ftree mating problem")))))
      (let ((ALLOW-NONLEAF-CONNS (apply #'append (mapcar #'(lambda (x)
							     (list (ftree-name (car x))
								   (ftree-name (cdr x))))
							 ftree-clist))))
	(declare (special ALLOW-NONLEAF-CONNS))
	(let* ((jform (ftree-to-jform node))
	       (clist nil)
	     (i 1)
	     (cgraph (make-hash-table :test #'equal)))
	(when (and (conjunction-p jform) (not (conjunction-components jform)))
	  (return-from check-ftree-mating t))
	(msgf ftree-clist t)
	(dolist (p ftree-clist)
	  (setf (gethash (cons (ftree-name (car p)) (ftree-name (cdr p)))
			 cgraph)
	    i)
	  (unless (wffeq-ab (neg-norm (ftree-shallow (car p)))
			    (neg-norm 
			      (if (eq (ftree-positive (car p))
				      (ftree-positive (cdr p)))
				  (cons 'NOT (ftree-shallow (cdr p)))
				(ftree-shallow (cdr p)))))
	    (throwfail "shallows in conn do not match" p))
	  (push i clist)
	  (incf i))
	(let ((path (find-next-openpath jform nil cgraph clist)))
	  (if path
	      (throwfail "path " path " is open " t " with clist " ftree-clist)
	    t)))))))

