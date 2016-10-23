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

(deffile nat-etr
  (extension lisp)
  (part-of etr-nat)
  (mhelp "Functions for translating from natural deduction
proofs to expansion proofs."))


;;; LINE-NODE-LIST is a list of conses, each pair consisting of a line in
;;; the proof and the corresponding etree node.
;;; MATE-LIST is a list of conses of etree nodes which are mated by
;;; the proof.

(defun nat-etree-old (proof)
  (let ((dproof (copy-nproof proof))
	(lines nil)
	(assertion nil)
	(line-node-list nil)
	(*ignore-statuses* t)
	(mate-list nil))
    (declare (special line-node-list mate-list))
    (when (proof-plans dproof) 
      (throwfail "Natural deduction proof is not complete."))
    (setq lines (proof-lines dproof))
    (when (null lines)
      (throwfail proof " has no lines in it."))
    (same-ify)
;    (pall)
    (setq assertion (proof-assertion dproof))
    (initialize-etree-counters)
    (setq current-eproof (make-eproof) master-eproof current-eproof)
    (setf (get (eproof-name current-eproof) 'EPROOF) current-eproof) ; 9/13/01
    (push (eproof-name current-eproof) *eproof-list*) ; 9/13/01
    (initialize-mating-search )
    (update-line-node-list (car (last lines))
			   (make-leaf :shallow assertion
				      :positive nil
				      :parent nil))
    (setf (eproof-etree current-eproof)
	  (justified-by-node (car (last lines))))
    (update-status nil (eproof-etree current-eproof) 1)
    (init-symmetry (eproof-etree current-eproof) current-eproof)
    (nat-xlate (car (last lines)))
    (setf (eproof-etree current-eproof)
	  (justified-by-node (car (last lines))))
    (let ((*ignore-statuses* nil))
      (update-statuses (eproof-etree current-eproof)))
    (setf (eproof-jform current-eproof)
	  (etree-to-jform (eproof-etree current-eproof)))
    (setf (eproof-leaf-list current-eproof)
	  (find-etree-nodes #'leaf-p (eproof-etree current-eproof)))
    ;(setf (eproof-merged current-eproof) t)
    (make-cgraph-and-mating mate-list)
    (let ((*ignore-statuses* nil))
      (merge-tree))
    (msgf "Proof has been translated -- use ETREE-NAT to reverse the
process." t))
)

(defun copy-nproof (nproof)
  (let ((newproof (gensym "PROOF"))
	(key (mapcar #'(lambda (x) (cons x (copy-symbol x)))
		     (proof-lines dproof))))
    (setf (proof-plans newproof) (sublis key (proof-plans nproof)))
    (setf (proof-key newproof) key)
    (setf (proof-linealiases newproof) (sublis key (proof-linealiases nproof)))
    (setf (proof-lines newproof) (sublis key (proof-lines nproof)))
    (setf (proof-nodealiases newproof) (sublis key (proof-nodealiases nproof)))
    (setf (proof-assertion newproof) (proof-assertion nproof))
    (setf (nextplan-no newproof) (nextplan-no nproof))
    (dolist (x key) 
      (let ((new (cdr x)) (old (car x)))
      (setf (line-justification new)
	(copy-tree (line-justification old)))
      (setf (line-just-lines new)
	(sublis key (line-just-lines new)))
      (setf (line-assertion new) (line-assertion old))
      (setf (line-hypotheses new) (sublis key (copy-list
					       (line-hypotheses
						old))))
      (setf (line-linenumber new) (line-linenumber old))))
    newproof))


(defun make-cgraph-and-mating (new-conn-list)
  (let ((new-mating (init-mating)))
    (setf (mating-completep new-mating) t)
    (do ((jform (eproof-jform current-eproof))
	 (conn-list (cdr new-conn-list) (cdr conn-list))
	 (conn (car new-conn-list) (car conn-list))
	 (new-conn-list nil)
	 (cgraph (cgraph)))
	((null conn) 
	 (setf (mating-clist new-mating) new-conn-list)
	 (push new-mating (mating-list)) 
	 (setq active-mating new-mating))
	(let ((first (find-jform-name (etree-name (car conn)) jform))
	      (sec (find-jform-name (etree-name (cdr conn)) jform))
	      (ctr nil))
	  (setq ctr (insert-in-cgraph (cons (etree-name (car conn)) 
					    (etree-name (cdr conn)))
				      (cons first sec) nil nil cgraph))
	  (push ctr new-conn-list)))))

;;; NODE should be the node which should be associated with LINE.

(defun update-line-node-list (line node)
  (declare (special line-node-list mate-list))
  (let ((current-value (justified-by-node line)))
    (if current-value
	(progn
	  (setf (etree-parent node) (etree-parent current-value))
	  (when (etree-parent node)
		(setf (etree-components (etree-parent node))
		      (nsubstitute node current-value
				   (etree-components (etree-parent node)))))
	  (setq line-node-list 
		(mapcar #'(lambda (x) (if (eq (cdr x) current-value)
					  (cons (car x) node)
					x))
			line-node-list))
	  (setq mate-list (mapcar #'(lambda (x)
				      (if (eq (car x) current-value)
					  (cons node (cdr x))
					  (if (eq (cdr x) current-value)
					      (cons (car x) node)
					      x)))
				  mate-list)))
      (push (cons line node) line-node-list)
)))

(defun justified-by-node (line)
  (declare (special line-node-list))
  (cdr (assoc line line-node-list)))

;(defun cl-user::show-vp ()
;  (display-vp-diag (etree-to-jform (eproof-etree current-eproof))))



(defun xlate-rulep (line just-lines)
  (declare (special mate-list line-node-list))
  (let* ((node (justified-by-node line))
	 (topnode nil)
	 (supports nil)
	 (left nil))
    (cond (node
	  (update-line-node-list line (deepen-to-literals-rulep node))
	  (setq node (justified-by-node line))
	  (dolist (just just-lines)
	    (unless (justified-by-node just) (nat-xlate just))
	    (update-line-node-list just
				   (deepen-to-literals-rulep 
				     (justified-by-node just))))
	  (setq supports (mapcar #'justified-by-node just-lines))
	  (setq topnode (make-implication :positive nil :junctive 'con))
	  (setq left (cond ((cdr supports)
			    (make-econjunction :positive t :junctive 'con
					       :components supports))
			   (supports (car supports))
			   (t (make-true :positive t :junctive 'CON)))) ; cebrown 11/19/00 added :junctive 'CON
	  (setf (etree-components topnode) (list left node))
	  (setq mate-list 
		(append 
		  (mapcar #'(lambda (x) 
			      (cons (find-etree-node-name (car x) topnode)
				    (find-etree-node-name (cdr x) topnode)))
			  (find-rulep-mating (etree-to-jform topnode)))
			mate-list)))
	  ((wffeq 'falsehood (line-assertion line))
	   (dolist (just just-lines)
		   (unless (justified-by-node just) (nat-xlate just))
		   (update-line-node-list just
					  (deepen-to-literals-rulep 
					    (justified-by-node just))))
	   (setq supports (mapcar #'justified-by-node just-lines))
	   (setq topnode (make-implication :positive nil :junctive 'con))
	   (setq node (make-false :positive nil :junctive 'CON :parent topnode)) ; cebrown 11/19/00 added :junctive 'CON
	   (setq left (cond ((cdr supports)
			     (make-econjunction :positive t :junctive 'con
						:components supports))
			    (supports (car supports))
			    (t (make-true :positive t :junctive 'CON)))) ; cebrown 11/19/00 added :junctive 'CON
	   (setf (etree-components topnode) (list left node))
	   (setq mate-list 
		 (append 
		   (mapcar #'(lambda (x) 
			       (cons (find-etree-node-name (car x) topnode)
				     (find-etree-node-name (cdr x) topnode)))
			   (find-rulep-mating (etree-to-jform topnode)))
		   mate-list)))

	;;; here as an immense simplification, assume that the line
	;;; is a conjunct of a single line used in rulep

	(t
	  (when (or (cdr just-lines) (null just-lines))
	    (throwfail "Whoa!!!  Problem in Xlate-rulep."))
	  (setq supports (ml::find-all-conjuncts (line-assertion 
						   (car just-lines))))
	  (unless (member (line-assertion line) supports :test #'wffeq-ab)
	    (throwfail "Whoa!!!  Problem in Xlate-rulep (part 2).  Line "
		       (linealias line)"."))
	  (unless (justified-by-node (car just-lines))
	    (nat-xlate (car just-lines)))
	  (setq topnode (justified-by-node (car just-lines)))
	  (update-line-node-list (car just-lines)
				 (deepen-to-literals-rulep topnode))
	  (setq topnode (justified-by-node (car just-lines)))
	  (update-line-node-list
	    line
	    (do ((nodes nil (cdr nodes))
		 (node topnode (car nodes)))
		((wffeq-ab (line-assertion line)
			   (get-shallow node)) node)
	      (setq nodes (append nodes (etree-components node)))))
	  ))))

#+comment
(defun deepen-leaf-node-rulep (node)
  (let ((wff (get-shallow node))
	(free-vars (etree-free-vars node))
	(parent (etree-parent node))
	(positive (positive-p node))
	(rename-all-bd-vars t)
	newnode)
    (declare (special rename-all-bd-vars))
    (setq newnode
	  (cond 
		((not-p wff)
		 (create-negation-node wff positive free-vars parent node))
		((and-p wff)
		 (create-econjunction-node wff positive free-vars parent node))
		((or-p wff)
		 (create-edisjunction-node wff positive free-vars parent node))
		((implies-p wff)
		 (create-implication-node wff positive free-vars parent node))
		((equiv-p wff)
		 (create-rewrite-node
		  wff positive (rewrite-equiv wff positive) 'equiv-implics
		  free-vars parent node))
		((wffeq wff 'TRUTH)
		 (create-true-node wff positive free-vars parent node))
		((wffeq wff 'FALSEHOOD)
		 (create-false-node wff positive free-vars parent node))
		(t nil)))
    (cond (newnode
	    (update-global-lists node newnode)
	    newnode)
	  (T node))))


(defun deepen-to-literals-rulep (gwff)
  (let* ((parent (etree-parent gwff))
	 (n (if parent (position gwff (etree-components parent)))))
    (mapc #'deepen-to-literals*-rulep
	  (find-etree-nodes #'leaf-p gwff))
    (if n (nth n (etree-components parent)) (eproof-etree current-eproof)))
 )


(defun deepen-to-literals*-rulep (gwff)
  (let ((newnode (if (leaf-p gwff) (deepen-leaf-node-rulep gwff) gwff)))
    (dolist (node (etree-components newnode) newnode)
      (deepen-to-literals*-rulep node))
))


(defun find-rulep-mating (jform)
  (let ((prop-classes nil)
	(*rulep-mate-list* nil))
    (declare (special prop-classes *rulep-mate-list*))
    (valid-p-main jform)
    *rulep-mate-list*))

(context search-suggestions)

(defmexpr auto-suggest
  (mhelp "Given a completed natural deduction proof (which must
be the current dproof; use RECONSIDER to return to an old proof),
suggest flag settings for an automatic proof of the same theorem.

This will also automatically remove all uses of SUBST=
and SYM= from the proof (you will be prompted before this
happens, as it permanently modifies the proof).

This will show all of the instantiations (and primitive substitutions)
that are necessary for the proof, and suggest settings for 
NUM-OF-DUPS, MAX-MATES, DEFAULT-MS, MAX-PRIM-DEPTH, MAX-PRIM-LITS 
and REWRITE-DEFNS"))

(defun identical-rulep (l1 l2)
  (let ((j1 (line-justification l1))
	(j2 (line-justification l2)))
    (and (string= (car j1) "RuleP")
	 (string= (car j2) "RuleP")
	 (null (set-difference (caddr j1) (caddr j2) :test 'eq)))))

(eval-when (load compile eval)
(defvar *suggest-list* nil)
(defvar *instantiated* nil)
(defvar *return* nil))


(defun auto-suggest (&optional (define-mode t))
  (declare (special *suggest-list*))
  (when (get dproof 'plans) (throwfail "AUTO-SUGGEST only works on completed proofs."))
  (let* ((assert-lines (remove-if-not #'(lambda (x) (string= (car (line-justification x)) 
							    "Assert"
							   :start1 0 :end1 (min 6 (length (car (line-justification x))))))
				     (get dproof 'lines)))
	 (bad-asserts (remove-if #'(lambda (x) (member (car (line-justification x)) 
							   '("Assert REFL=" "Assert SYM=")
							   :test 'string=))
				     assert-lines))
	 (rewrites (remove-if-not #'(lambda (x) (member (car (line-justification x)) 
							    '("Rewrite" "Rewrites")
							   :test 'string=))
				     (get dproof 'lines))))
    (when bad-asserts 
	  (msgf "Line" (if (cdr bad-asserts) "s " " ") (l (mapcar 'linealias bad-asserts))
		(if (cdr bad-asserts) "are assertions " "is an assertion ") "other than REFL= or SYM=." t
		"Because of this, TPS cannot produce this proof automatically.")
	  (throwfail "Unable to continue."))
    (when rewrites
	  (msgf "Line" (if (cdr rewrites) "s " " ") (l (mapcar 'linealias rewrites))
		(if (cdr rewrites) "are " "is ") "justified by rewrite rules." t
		"Because of this, TPS cannot produce this proof automatically.")
	  (throwfail "Unable to continue.")))
  (let ((subst=-lines (remove-if-not #'(lambda (x) (member (car (line-justification x)) 
							   '("Subst=" "Sub=" "Sym=" "Assert SYM=")
							   :test 'string=))
				     (get dproof 'lines))))
    (when subst=-lines
	  (msgf "Some lines in this proof are justified by SUBST= or SYM=" t 
		"AUTO-SUGGEST may not work properly until they are removed." t t)
	  (if (query "Remove these lines? (This will permanently modify the proof.)" nil)
	      (remove-subst=-lines subst=-lines)
	    (unless (query "Continue without removing these lines?" t)
		    (throwfail "Unable to continue.")))))
  (let* ((lines (get dproof 'lines))
	 (ilines (remove-if-not #'(lambda (x) (member (car (line-justification x)) '("UI" "EGen") :test 'string=))
				lines))
	 (non-ilines (remove-if 
		      #'(lambda (x) (member (car (line-justification x)) 
					    '("UI" "EGen" "Conj" "EquivImp" "ImpEquiv" "Equality"
					      "Deduct" "AB" "Lambda=" "Lambda*" "Eta Rule" "Beta Rule"
					      "NegElim" "NegIntro" "Indirect" "Assoc" "Idisj-L" "Idisj-R"
					      "Case 1" "Case 2" "Case 3" "Case 4" "Choose" "Ugen") 
					    :test 'string=))
				lines))
	 (non-ilines (remove-duplicates non-ilines :test 'identical-rulep))
	 (non-ijusts (sort (mapcar #'linealias (apply 'append (mapcar #'(lambda (x) (caddr (line-justification x))) 
								      non-ilines)))
			   '< ))
	 (ijusts (sort (mapcar #'(lambda (x) (cons (linealias (caaddr (line-justification x)))
						   (cadr (line-justification x))))
			       ilines)
		 '< :key 'car))
	 (rd (get-rewrite-defns lines))
	 (flist nil)
	 (nod 0)
	 (mode-flags nil))
    (declare (special mode-flags))
    (msgf "WARNING: the following are only estimates of the correct values!" t t)
    (do ((l ijusts (cdr l))
	 (temp-nod 0)
	 (rlist nil))
	((null l) 
	 (msgf "NUM-OF-DUPS should be " nod (if rlist ":" " ") t)
	 (push (list 'num-of-dups nod) mode-flags)
	 (push nod *suggest-list*)
	 (dolist (l (reverse rlist)) (msgf " " (1+ (car l)) " instance" (if (> (car l) 0) "s " " ") "of line " (cdr l))
		 (msgf "  (Instantiated with " )
		 (setq flist (append (mapcar 'cadr (remove-if-not #'(lambda (x) (= (car x) (cdr l))) ijusts)) flist))
		 (dolist (g (mapcar 'cadr (remove-if-not #'(lambda (x) (= (car x) (cdr l))) ijusts)))
			 (msg "\"" (g . gwff) "\" "))
		 (msg ")")))
	(if (eq (caar l) (caadr l))
	    (progn (incf nod) (incf temp-nod))
	  (progn
		(push (cons temp-nod (caar l)) rlist)
		(setq temp-nod 0))))
    (msgf t)
    (push rd *suggest-list*)
    (msgf "REWRITE-DEFNS should be (EAGER")
    (when (car rd) (msg " (LAZY2 " (l (car rd)) ")"))
    (when (cadr rd) (msg " (NONE " (l (cadr rd)) ")"))
    (msg ")" t t)
    (push (list 'rewrite-defns 
		(if (car rd)
		     (if (cadr rd)
			 (list 'EAGER (cons 'LAZY2 (car rd)) (cons 'NONE (cadr rd)))
		       (list 'EAGER (cons 'LAZY2 (car rd))))
		  (if (cadr rd)
		      (list 'EAGER (cons 'NONE (cadr rd)))
		     (list 'EAGER))))
	  mode-flags)
    (do ((l non-ijusts (cdr l))
	 (mm 1)
	 (temp-mm 1)
	 (rlist nil))
	((null l) 
	 (msgf "MAX-MATES in MS88-style should be " mm (if (> mm 1) ":" " ") 
	       " (in MS90-3 style, no more than " (* mm (1+ nod)) ")" t)
	 (push mm *suggest-list*)
	 (if (> nod 0) (push (list 'max-mates (* mm (1+ nod))) mode-flags)
	   (push (list 'max-mates mm) mode-flags))
	 (dolist (l (reverse rlist)) (msgf " " (car l) " uses of line " (cdr l))))
	(if (eq (car l) (cadr l))
	    (incf temp-mm)
	  (progn
	    (when (> temp-mm mm) (setq mm temp-mm))
	    (when (neq temp-mm 1) 
		  (push (cons temp-mm (car l)) rlist)
		  (setq temp-mm 1)))))
    (msgf t)
    (msv-find-depths flist nod)
    (when (and define-mode (query "Do you want to define a mode with these settings?" t))
	  (let ((symb (intern (concatenate 'string "MODE-" (princ-to-string dproof)
						   "-SUGGEST"))))
	    (do ((name nil))
		((not (or (null name) (memq name global-modelist)))
		 (eval `(defmode ,name ,(cons 'flag-settings (core::flagsort mode-flags))
			  (mhelp "Mode created by AUTO-SUGGEST.")))
		 (reorganize))
		(prompt-read name nil (msgf "Name for mode? ") 'symbol symb
			     ((? (msgf "Provide a name for the new mode."))
			      (?? (mhelp 'tps-mode))))
		(when (and (memq name global-modelist)
			   (query "That mode already exists. Overwrite it?" t))
		      (dolist (prop (plist name))
			      (remprop name prop))
		      (setq global-modelist (delete name global-modelist))))))))

(defun msv-find-depths (flist nod &optional (oldflist nil) (again nil) (ok-before nil))
  (declare (special *suggest-list* mode-flags))
  (when again
	(msgf t "  Removing the above non-standard primsubs, we get new substitutions:" t)
	(dolist (elt (mapcar 'cons flist oldflist))
		(unless (wffeq-ab (car elt) (cdr elt))
			(msgf "  " ((cdr elt) . gwff) " ---> " ((car elt) . gwff))))
	)
  (let ((foolist nil)
	(msv 2)
	(mult (max 1 (find-skolem-depth (get dproof 'assertion))))
	(pr97b nil)
	(pr-bdtypes nil)
	(pr-depth 0)
	(pr-lits 0)
	(pr-binders nil)
	(pr-consts nil)
	(pr nil))
  (do ((r flist (cdr r)))
      ((null r)
       (setq msv (* msv mult))
       (unless pr
	       (msgf "MAX-SUBSTS-VAR should be " msv t t)
	       (push (list 'MAX-SUBSTS-VAR msv) mode-flags)
	       (push (list 'MAX-SUBSTS-QUICK msv) mode-flags))
       (unless again (msgf "Primsubs are" (if pr " " " not ") "required." t t)
	       (push (list msv pr) *suggest-list*))
       (when pr
	     (let* ((foo (find-pr97-subs foolist (1+ pr-depth) pr-lits (if again 'pr95 'pr97) pr-bdtypes))
		    (foo2 (when (and (not again) (or pr-consts pr-binders))
				;there are some special primsubs to be done
				(find-pr97-subs foolist (1+ pr-depth) pr-lits 'pr97b pr-bdtypes)))
		    (all-ok (null (remove-if-not 'null foo))))
	       (if all-ok (msgf (if again "  " "")
				"All of the required primsubs can" (if (and again ok-before) " also" "") 
				" be generated by " (if again "PR95" "PR97") t t)
		 (progn (msgf "  The following primsubs cannot be generated by " (if again "PR95" "PR97") ":" t)
			(do ((f1 foolist (cdr f1))
			     (f2 foo (cdr foo)))
			    ((or (null f1) (null f2)) (msgf t))
			    (when (null (car f2))
				  (msgf "  " ((car f1) . gwff))))))
	       (when (and (not again) (not all-ok) (or pr-consts pr-binders))
		     (setq all-ok (null (remove-if-not 'null foo2)))
		     (when all-ok (msgf (if again "  " "")
					"All of the required primsubs can be generated by PR97B" t t))
		     (setq pr97b all-ok))
	       (when (and all-ok (not ok-before)) 
		     (push (list 'primsub-method (if pr97b 'pr97b (if again 'pr95 'pr97))) mode-flags)
		     (msgf "PRIMSUB-METHOD should be " (if pr97b "PR97B" (if again "PR95" "PR97")) t t))
	       (when (and again (not all-ok) (not ok-before))
		     (msgf "TPS cannot generate the required primsubs!"))
	       (when all-ok 
		     (unless (assoc 'max-substs-var mode-flags)
			     (push (list 'max-substs-var msv) mode-flags)
			     (push (list 'max-substs-quick msv) mode-flags))
		     (msgf (if pr97b "In PR97B, " (if again "  In PR95, " "In PR97, "))
			   "MAX-SUBSTS-VAR should be " msv t)
		     (if pr-bdtypes
			 (progn
			   (setq pr-bdtypes (remove-duplicates (mapcar #'core::type-to-string-2 pr-bdtypes) :test 'string=))
				(msgf "  " (if pr97b "In PR97B" (if again "In PR95" "In PR97"))
				      ", primsubs with at most " pr-depth " quantifier" (if (> pr-depth 1) "s " " ")
				      "and at most " pr-lits " literal" (if (> pr-lits 1) "s " " ") t
				      "  are used, and " 
				      (if (> pr-depth 1) "these quantifiers range " "this quantifier ranges ")
				      "over bound variables of type" (if (> (length pr-bdtypes) 1) "s " " "))
				(dolist (tp pr-bdtypes) (msg tp " ")))
		       (msgf "  " (if pr97b "In PR97B, n" (if again "In PR95, n" "In PR97, n"))
			     "o quantified primsubs are used."))
		     (setq pr-consts (remove-duplicates pr-consts))
		     (setq pr-binders (remove-duplicates pr-binders))
		     (when pr-consts
			   (msgf "  The following constants are used in primsubs but cannot be generated" t
				 "  by primsub routines except PR97*: " (l pr-consts)))
		     (when pr-binders
			   (msgf "  The following binders are used in primsubs but cannot be generated" t
				 "  by primsub routines except PR97*: " (l pr-binders)))
		     (msgf (if pr97b "In PR97B, " (if again "In PR95, " "In PR97, "))
			   "MAX-PRIM-DEPTH should be " (1+ pr-depth) t)
		     (unless (assoc 'max-prim-depth mode-flags)
			     (push (list 'max-prim-depth (1+ pr-depth)) mode-flags)
			     (push (list 'min-prim-depth 1) mode-flags))
		     (when pr-bdtypes 
			   (unless (assoc 'prim-bdtypes mode-flags)
				   (push (list 'prim-bdtypes (mapcar 'princ-to-string pr-bdtypes)) mode-flags))
			   (msgf (if pr97b "In PR97B, " (if again "In PR95, " "In PR97, "))
				 "PRIM-BDTYPES should be " (l (mapcar 'princ-to-string pr-bdtypes)) t)))
	       (when (and (not again) (or (not all-ok) pr-consts pr-binders))
		     (msv-find-depths (mapcar #'(lambda (x) (repeated-instantiate x)) flist) nod flist t all-ok))))
       (unless again
	       (if pr
		   (if (> nod 0)
		       (progn (setq mode-flags 
				    (append `((default-ms ms91-7) (max-prim-lits ,pr-lits)
					      (min-prim-lits 1) (prim-bdtypes-auto ignore)) mode-flags))
			      (msgf t "DEFAULT-MS should be MS91-7" t "MAX-PRIM-LITS should be " pr-lits t
				    "PRIM-BDTYPES-AUTO should be IGNORE"))
		     (progn (setq mode-flags 
				  (append `((default-ms ms91-6) (max-prim-lits ,pr-lits)
					    (min-prim-lits 1) (prim-bdtypes-auto ignore)) mode-flags))
			    (msgf t "DEFAULT-MS should be MS91-6" t "MAX-PRIM-LITS should be " pr-lits t
				  "PRIM-BDTYPES-AUTO should be IGNORE")))
		 (if (> nod 0)
		     (progn (push '(default-ms ms90-3) mode-flags)
			    (msgf t "DEFAULT-MS should be MS90-3"))
		   (progn (push '(default-ms ms88) mode-flags)
			  (msgf t "DEFAULT-MS should be MS88"))))))
      (multiple-value-bind (temp-msv temp-lits primsubs-needed)
			   (find-depth-required (car r))
			   (when (> temp-msv msv) (setq msv temp-msv))
			   (when (> temp-lits pr-lits) (setq pr-lits temp-lits))
			   (when (> temp-lits 1) (push (car r) foolist))
			   (when (or (cadr primsubs-needed) (car primsubs-needed))
				 (setq pr t)
				 (when (> (length (car primsubs-needed)) pr-depth)
				       (setq pr-depth (length (car primsubs-needed))))
				 (dolist (bd (car primsubs-needed))
					 (unless (memq (cdr bd) '(FORALL EXISTS))
						 (push (cdr bd) pr-binders))
					 (push (type (car bd)) pr-bdtypes))
				 (dolist (bd (cadr primsubs-needed))
					 (unless (memq bd '(AND OR))
						 (push bd pr-consts))))))))

(defun find-skolem-depth (gwff)
  (mate-wff-prefix gwff t nil)
  (apply 'max (mapcar 'cdr (eproof-skolem-constants current-eproof))))

(defun find-pr97-subs (gwfflist pr-depth pr-lits pr-method pr-bdtypes)
  (clrhash prim-hashtable)
  (let* ((returnlist nil)
	 (max-prim-depth (max 1 pr-depth))
	 (min-prim-depth 1)
	 (max-prim-lits (max 1 pr-lits))
	 (min-prim-lits 1)
	 (primsub-method pr-method)
	 (prim-bdtypes (mapcar #'(lambda (x) (if (or (listp x) (symbolp x)) x (get-stringtype x))) pr-bdtypes))
	 (prlist nil)
	 (blanklist nil)
	 (displaywff t)
	 (ppwfflag nil)
	 (prev-node nil)
	 (nodestack nil)
	 (cmdstack nil)
	 (current-topnode nil)
	 (strong-defaults 'strong-mate-defaults)
	 (printtypes printtypes)
	 (truegwff (get dproof 'assertion)))
    (declare (special blanklist displaywff printdepth ppwfflag  prev-node nodestack
		      cmdstack strong-defaults  printtypes current-eproof
		      current-topnode))
    (mate-wff-prefix truegwff t nil)
    (when (null prim-bdtypes) 
	  (setq prim-bdtypes '(I)))
    (setq prlist 
      (apply #'append ; to fix a bug with dont-apply-prim-subs - cebrown 5/11/00
	     (mapcar #'dont-apply-prim-subs (find-ho-exps (find-exp-nodes current-topnode)))))
    (setq prlist (mapcar #'(lambda (x) (blank-out-parts-of (drop-lambdas (get x 'represents))))
			 (remove-if-not 'symbolp (apply 'append prlist))))
    (dolist (gwff gwfflist returnlist)
	    (let ((newgwff (blank-out-parts-of (drop-lambdas gwff))))
	      (push 
	       (car (remove-if-not #'(lambda (x) (wffeq-ab x newgwff)) prlist))
	       returnlist)))))

(defun drop-lambdas (gwff)
  (do ((g gwff (cdr g)))
      ((or (not (boundwff-q g)) (neq (cdar g) 'LAMBDA)) g)))

(defun repeated-instantiate (gwff)
  (do ((g gwff (neg-norm2 (lnorm (rew-equiv (lnorm (instantiate-all (instantiate-equalities g) '(equiv)))))))
       (oldg nil g))
      ((wffeq-ab g oldg) g)))

(defun rew-equiv (gwff)
  (cond ((or (lsymbol-p gwff) (not (consp gwff)))
	 (if (eq gwff 'equiv)
	     (getrwff "lambda x(o) lambda y(o) . [x implies y] AND [y IMPLIES x]")
	   gwff))
	(t (cons (rew-equiv (car gwff)) (rew-equiv (cdr gwff))))))

(defun neg-norm2 (gwff)
  (do ((g gwff (cdr g))
       (b nil))
      ((or (not (boundwff-q g)) (neq (cdar g) 'LAMBDA))
       (append (reverse b) (neg-norm g)))
      (push (car g) b)))

(defun find-depth-required (gwff)
  (let ((binders nil)
	(wff-pieces nil)
	(consts nil)
	(gwff2 nil))
    (setq gwff2 (do ((g gwff (cdr g)))
		    ((or (not (boundwff-q g)) (neq (cdar g) 'LAMBDA)) g)))
    (setq gwff2 (do ((g gwff2 (cdr g)))
		    ((not (boundwff-q g)) g)
		    (push (car g) binders)))
    (do ((g (list gwff2)))
	((null g))
	(let ((wff (pop g)))
	  (if (or (infix-p wff) (equality-p wff))
	      (progn
		(push (cdar wff) g)
		(push (cdr wff) g)
		(push (caar wff) consts))
	    (if (and (consp wff) (or (equality-p (car wff)) (infix-p (car wff))))
		(progn (push (car wff) consts)
		       (push (cdr wff) g))
	      (if (boundwff-q wff)
		  (let ((wff2 (do ((g2 wff (cdr g2)))
				  ((not (boundwff-q g2)) g2)
				  (push (car g2) binders))))
		    (push wff2 g))
		(push wff wff-pieces))))))
    (values (apply 'max (mapcar 'fdr-real wff-pieces)) (length wff-pieces) (list binders consts))))

(defun fdr-real (gwff)
  (if (consp gwff) (+ (fdr-real (car gwff)) (fdr-real (cdr gwff)))
    (if (null gwff) 0 1)))

; This is the old version.  The new version is a complete rewrite
; which does not rely on search.
;(defun remove-subst=-lines (lines)
;  (declare (special search-time-limit max-mates num-of-dups))
;  (dolist (line lines)
;	  (when (memq line (get dproof 'lines)) ;may have been deleted during cleanup from an earlier line
;	  (let* ((just (caddr (get line 'justification))) ;just is a 2-elt list; the second elt is the equality
;		 (just2 (line-justification line))
;		 (remove-leibniz nil) ; don't want the proof to contain a new subst= !
;		 (rewrite-equalities 'all) ; just in case...
;		 (gwff (get (car just) 'assertion)))
;	    (let ((foo nil)
;		  (crashed t))
;	      (unwind-protect
;		  (progn 
;		    (msgf "Trying to justify " t
;			  ((get line 'assertion) . gwff) t
;			  "from " (gwff . gwff) t
;			  "and " ((get (cadr just) 'assertion) . gwff) t)
;		    (plan line)
;		    (sponsor line (list (car just)))
;		    (push (car just) foo)
;		    (sponsor line (cdr just))
;		    (push (cadr just) foo);)
;		    (introduce-gap line 50)
;		    (diy-lemma line (remove-if #'null foo) nil (get-useful-range line))
;		    (setq crashed nil))
;		(when crashed 
;		  (setf (line-justification line) just2)
;		  (setf (proof-plans dproof) nil)
;		  (remove-unnecessary-gaps)))))))
;  (unless (proof-plans dproof)
;    (apply-tactic 'make-nice :use 'nat-ded :mode 'auto)
;    (msgf t t t t "All instances of SUBST= and SYM= removed." t t t t)
;    (tpsbell) (finish-output) (sleep 1/2) (tpsbell) (finish-output) (sleep 1/2) (tpsbell)))

(defun remove-subst=-lines (lines)
  (dolist 
   (line lines)
   (when (memq line (get dproof 'lines)) ; may have been deleted during cleanup from an earlier line
	 (cond ((or (equal (car (line-justification line)) "Subst=")
		    (equal (car (line-justification line)) "Sub="))
		(remove-subst=-line line))
	       ((equal (car (line-justification line)) "Sym=")
		(remove-sym=-line line))
	       ((equal (car (line-justification line)) "Assert SYM=")
		(remove-assert-sym=-line line))))))

(defun remove-subst=-line (line)
  (let* ((ln (linealias line))
	 (justlines (caddr (line-justification line)))
	 (preline (car justlines))
	 (eqline (cadr justlines))
	 (eqas (line-assertion eqline))
	 (term1 (cdar eqas))
	 (term2 (cdr eqas))
	 (wff2 (line-assertion line))
	 (wff1 (line-assertion preline))
	 (rule-hupper (line-hypotheses line)) ; actually, I think free-in-hyps should use rule-hlower (see THM508) - cebrown
	 (pvs (intersection (union (free-vars-of term1) (free-vars-of term2))
			    (free-in-hyps))))
    (cond ((subst-occs term1 wff1 term2 wff2 pvs)
	   (let ((qvs (bounds-in-subst-occs term1 wff1 term2 wff2
					    (union (free-vars-of term1)
						   (free-vars-of term2)))))
; if qvs is nil, then we can use Leibniz directly.  Otherwise, we first
; need length(qvs) applications of extensionality, then obtain the conclusion
; by a substitution of equality of functions for which qvs will be nil.
	     (if qvs
		 (remove-subst=-line
		  (apply-extensionality-before-subst=
		   line eqline preline qvs term1 wff1 term2 wff2))
	       (let* ((x (fresh-var-1 (type term1)))
		      (qsub (cons (cons x 'LAMBDA)
				  (subst-occs-template x term1 wff1 term2 wff2)))
		      (rewrite-equalities 'eager)
		      (ei0 (instantiate-top-equality eqas))
		      (ei (lcontr (cons (lcontr (car ei0)) (cdr ei0)))))
		 (introduce-gap line 3)
		 (comdecode (list 'cl-user::plan (+ ln 3)))
		 (comdecode (list 'cl-user::lemma (+ ln 3) (+ ln 2)
				  (list 'quote
					(cons (cons 'IMPLIES wff1) wff2))
				  '$ '$ '$))
		 (comdecode (list 'cl-user::mp (+ ln 2) (+ ln 3) (linealias preline)
				  '$ '$ '$ '$ '$))
		 (comdecode (list 'cl-user::lemma (+ ln 2) (+ ln 1)
				  (list 'quote (lcontr (cons (cons (cons (bindvar ei)
									 'LAMBDA)
								   (cdr ei))
							     qsub)))
				  '$ '$ '$))
		 (comdecode (list 'cl-user::lambda* (+ ln 1) (+ ln 2)
				  '$ '$ '$ '$))
		 (comdecode (list 'cl-user::lemma (+ ln 1) ln
				  (list 'quote ei)
				  '$ '$ '$))
		 (comdecode (list 'cl-user::ui ln (+ ln 1) (list 'quote qsub)
				  '$ '$ '$ '$ '$))
		 (comdecode (list 'cl-user::equiv-eq (linealias eqline) ln
				  '$ '$ '$ '$))))))
	  ((subst-occs term2 wff1 term1 wff2 pvs)
	   (let ((qvs (bounds-in-subst-occs term1 wff1 term2 wff2
					    (union (free-vars-of term1)
						   (free-vars-of term2)))))
	     (if qvs
		 (remove-subst=-line
		  (apply-extensionality-before-subst=
		   line eqline preline qvs term1 wff1 term2 wff2))
	       (let* ((x (fresh-var-1 (type term1)))
		      (qsub (cons (cons x 'LAMBDA)
				  (cons 'NOT
					(subst-occs-template x term1 wff1 term2 wff2))))
		      (rewrite-equalities 'eager)
		      (ei0 (instantiate-top-equality eqas))
		      (ei (lcontr (cons (lcontr (car ei0)) (cdr ei0)))))
		 (introduce-gap line 3)
		 (comdecode (list 'cl-user::plan (+ ln 3)))
		 (comdecode (list 'cl-user::lemma (+ ln 3) (+ ln 2)
				  (list 'quote
					(cons (cons 'IMPLIES 
						    (cons 'NOT wff2))
					      (cons 'NOT wff1)))
				  '$ '$ '$))
		 (comdecode (list 'cl-user::rulep (+ ln 3) (list 'quote
							      (list (+ ln 2) (linealias preline)))))
		 (comdecode (list 'cl-user::lemma (+ ln 2) (+ ln 1)
				  (list 'quote (lcontr (cons (cons (cons (bindvar ei)
									 'LAMBDA)
								   (cdr ei))
							     qsub)))
				  '$ '$ '$))
		 (comdecode (list 'cl-user::lambda* (+ ln 1) (+ ln 2)
				  '$ '$ '$ '$))
		 (comdecode (list 'cl-user::lemma (+ ln 1) ln
				  (list 'quote ei)
				  '$ '$ '$))
		 (comdecode (list 'cl-user::ui ln (+ ln 1) (list 'quote qsub)
				  '$ '$ '$ '$ '$))
		 (comdecode (list 'cl-user::equiv-eq (linealias eqline) ln
				  '$ '$ '$ '$))))))
	  (t ; otherwise, it must be an iterated application of Rule R'
; so we make the iterated applications explicit
; In this case, either term1 is a subterm of term2 or term2 is a subterm
; of term1
	   (multiple-value-bind
	    (wffs eq-used justs)
	    (subst-occs-iterations wff1 wff2 term1 term2)
	    (comdecode (list 'cl-user::plan ln))
	    (let* ((gap (* (length (cdr wffs)) 2))
		   (nln (+ ln gap))
		   (tp (type term1))
		   (e (inherit-abbrev '= (cons (cons 'O tp) tp) (list tp)))
		   (new-subst=-lines nil))
	      (introduce-gap line gap)
	      (do ((rwffs (cdr (reverse wffs)) (cdr rwffs))
		   (req-used (reverse eq-used) (cdr req-used)))
		  ((null rwffs))
		  (comdecode (list 'cl-user::lemma nln (- nln 2)
				   (list 'quote
					 (car rwffs))
				   '$ '$ '$))
		  (comdecode (list 'cl-user::lemma nln (- nln 1)
				   (list 'quote
					 (cons (cons e (caar req-used))
					       (cdar req-used)))
				   '$ '$ '$))
		  (comdecode (list 'cl-user::subst= (- nln 1) nln (- nln 2)
				   '$ '$ '$ '$ '$ '$ '$))
		  (setq new-subst=-lines (list (numalias nln)))
		  (let ((j1 (car (member (caar req-used) justs
					 :test #'(lambda (x j)
						   (wffeq x (car j))))))
			(j2 (car (member (cdar req-used) justs
					 :test #'(lambda (x j)
						   (wffeq x (car j)))))))
		    (setq nln (- nln 2))
		    (let ((eln (- (+ nln (length j1) (length j2)) 1))
			  (small (car (last j2))))
		      (introduce-gap (numalias (+ nln 1)) (- (+ (length j1) (length j2)) 2)) 
		      (dolist (b (cdr j2))
			      (comdecode (list 'cl-user::lemma eln (- eln 1)
					       (list 'quote (cons (cons e (caar req-used)) b))
					       '$ '$ '$))
			      (comdecode (list 'cl-user::subst= (list 'quote (linealias eqline))
					       eln (- eln 1)
					       '$ '$ '$ '$ '$ '$ '$))
			      (setq new-subst=-lines (cons (numalias eln) new-subst=-lines))
			      (setq eln (- eln 1)))
		      (dolist (a (cdr j1))
			      (comdecode (list 'cl-user::lemma eln (- eln 1)
					       (list 'quote (cons (cons e a) small))
					       '$ '$ '$))
			      (comdecode (list 'cl-user::subst= (list 'quote (linealias eqline))
					       eln (- eln 1)
					       '$ '$ '$ '$ '$ '$ '$))
			      (setq new-subst=-lines (cons (numalias eln) new-subst=-lines))
			      (setq eln (- eln 1)))
		      (assert% 'CL-USER::REFL= eln))))
	      (cl-user::same-short (numalias nln) preline)
	      (remove-subst=-lines new-subst=-lines)))))))

; Starts with a justification of the form
; (eqline)  M = N  (or M = N)
; (preline) P[M]
; (line)    P[N]   Subst=
; where the variables in qvs are bound in some occurrences designated by P[ ]
; and forms the alternative subproof (where qvs is v1 . . . vn and v- is reverse qvs
; (eqline)  M = N (or with this and all equations below turned around)
;           [lambda v- M] v- = [lambda v- N] v-   (lambda*)
;           forall v1 "                               (ugen)
;           [lambda v- M] vn ... v2 = [lambda v- N] vn ... v2 (Ext=)
;                 . . .
;           [lambda v- M] = [lambda v- N]        (after n applications of ugen and Ext=)
; (preline) P[M]
;           P[[lambda v- M] v-]  (lambda*)
;           P[[lambda v- N] v-]  (subst=  with no variables bound in the context of replacement occurrences)
;           P[N]                   (lambda*)
(defun apply-extensionality-before-subst= (line eqline preline qvs term1 wff1 term2 wff2)
  (let* ((fterm1 (bind-var-wff-n 'LAMBDA qvs term1))
	 (fterm2 (bind-var-wff-n 'LAMBDA qvs term2))
	 (term1* (apply-wff-n fterm1 qvs))
	 (term2* (apply-wff-n fterm2 qvs))
	 (gap (+ 3 (* 2 (length qvs))))
	 (nln (+ (linealias line) gap)))
    (multiple-value-bind
     (wff1* wff2*)
     (replace-subst-occs term1 term1* wff1 term2 term2* wff2)
     (introduce-gap line gap)
     (comdecode (list 'cl-user::plan nln))
     (comdecode (list 'cl-user::lemma nln (- nln 1)
		      (list 'quote wff2*)
		      '$ '$ '$))
     (comdecode (list 'cl-user::lambda* (- nln 1) nln
		      '$ '$ '$ '$))
     (comdecode (list 'cl-user::lemma (- nln 1) (- nln 3)
		      (list 'quote (cons (cons 
					  (create-propsym
					   '= (cons (cons 'o (type fterm1))
						    (type fterm1)))
					  fterm1)
					 fterm2))
		      '$ '$ '$))
     (comdecode (list 'cl-user::lemma (- nln 1) (- nln 2)
		      (list 'quote wff1*)
		      '$ '$ '$))
     (comdecode (list 'cl-user::subst= (- nln 3) (- nln 1) (- nln 2)
		      '$ '$ '$ '$ '$ '$ '$))
     (comdecode (list 'cl-user::lambda* (linealias preline) (- nln 2)
		      '$ '$ '$ '$))
     (do* ((i (- nln 3) (- i 2))
	   (vl (reverse qvs) (cdr vl))
	   (fterm1- (cons fterm1 (car vl)) (cons fterm1- (car vl)))
	   (fterm2- (cons fterm2 (car vl)) (cons fterm2- (car vl))))
	  ((null vl))
	  (let ((e- 
		 (cons (cons
			(create-propsym
			 '=
			 (cons (cons 'O
				     (type fterm1-))
			       (type fterm1-)))
			fterm1-)
		       fterm2-)))
	    (comdecode (list 'cl-user::lemma i (- i 1)
			     (list 'quote (cons (cons (car vl) 'FORALL) e-))
			     '$ '$ '$))
	    (comdecode (list 'cl-user::ext= i (- i 1) (list 'quote (car vl)) '$ '$ '$ '$))
	    (comdecode (list 'cl-user::lemma (- i 1) (- i 2) 
			     (list 'quote e-) '$ '$ '$))
	    (comdecode (list 'cl-user::ugen (- i 1) (- i 2) '$ '$ '$ '$))))
     (comdecode (list 'cl-user::lambda* (linealias eqline) (- nln gap)
		      '$ '$ '$ '$))
     (numalias (- nln 1)))))

; returns values wff1* wff2*
; where these are the result of replacing term1 with term1* in wff1
; and term2 with term2* in wff2 in the occurrences where wff1 and wff2 differ
(defun replace-subst-occs (term1 term1* wff1 term2 term2* wff2)
  (cond ((label-q wff1)
	 (apply-label wff1
		      (replace-subst-occs term1 term1* wff1 term2 term2* wff2)))
	((label-q wff2)
	 (apply-label wff2
		      (replace-subst-occs term1 term1* wff1 term2 term2* wff2)))
	((wffeq wff1 wff2) (values wff1 wff2))
	((and (wffeq term1 wff1)
	      (wffeq term2 wff2))
	 (values term1* term2*))
	((and (wffeq term2 wff1)
	      (wffeq term1 wff2))
	 (values term2* term1*))
	((or (lsymbol-q wff1)
	     (lsymbol-q wff2))
	 (throwfail "Illegal application of subst="))
	((and (boundwff-q wff1)
	      (boundwff-q wff2)
	      (equal (binding wff1) (binding wff2))
	      (equal (bindvar wff1) (bindvar wff2)))
	 (multiple-value-bind
	  (bwff1* bwff2*)
	  (replace-subst-occs term1 term1* (cdr wff1) term2 term2* (cdr wff2))
	  (values (cons (car wff1) bwff1*)
		  (cons (car wff2) bwff2*))))
	((or (boundwff-q wff1)
	     (boundwff-q wff2))
	 (throwfail "Illegal application of subst="))
	(t 
	 (multiple-value-bind
	  (wff1*1 wff2*1)
	  (replace-subst-occs term1 term1* (car wff1) term2 term2* (car wff2))
	  (multiple-value-bind
	   (wff1*2 wff2*2)
	   (replace-subst-occs term1 term1* (cdr wff1) term2 term2* (cdr wff2))
	   (values (cons wff1*1 wff1*2) (cons wff2*1 wff2*2)))))))

(defun bounds-in-subst-occs (term1 wff1 term2 wff2 fvl &optional bvl)
  (cond ((label-q wff1)
	 (apply-label wff1
		      (bounds-in-subst-occs term1 wff1 term2 wff2 fvl bvl)))
	((label-q wff2)
	 (apply-label wff2
		      (bounds-in-subst-occs term1 wff1 term2 wff2 fvl bvl)))
	((wffeq wff1 wff2) nil)
	((and (wffeq term1 wff1)
	      (wffeq term2 wff2))
	 (intersection fvl bvl))
	((and (wffeq term2 wff1)
	      (wffeq term1 wff2))
	 (intersection fvl bvl))
	((or (lsymbol-q wff1)
	     (lsymbol-q wff2))
	 (throwfail "Illegal application of subst="))
	((and (boundwff-q wff1)
	      (boundwff-q wff2)
	      (equal (binding wff1) (binding wff2))
	      (equal (bindvar wff1) (bindvar wff2)))
	 (bounds-in-subst-occs term1 (cdr wff1) term2 (cdr wff2)
			       fvl (cons (bindvar wff1) bvl)))
	((or (boundwff-q wff1)
	     (boundwff-q wff2))
	 (throwfail "Illegal application of subst="))
	(t (union
	    (bounds-in-subst-occs term1 (car wff1) term2 (car wff2) fvl bvl)
	    (bounds-in-subst-occs term1 (cdr wff1) term2 (cdr wff2) fvl bvl)))))

; builds the body of the term to substitute for "q" (of Leibniz defn of equality)
(defun subst-occs-template (x term1 wff1 term2 wff2)
  (cond ((label-q wff1)
	 (apply-label wff1
		      (subst-occs-template x term1 wff1 term2 wff2)))
	((label-q wff2)
	 (apply-label wff2
		      (subst-occs-template x term1 wff1 term2 wff2)))
	((wffeq wff1 wff2) wff1)
	((and (wffeq term1 wff1)
	      (wffeq term2 wff2))
	 x)
	((and (wffeq term2 wff1)
	      (wffeq term1 wff2))
	 x)
	((or (lsymbol-q wff1)
	     (lsymbol-q wff2))
	 (throwfail "Illegal application of subst="))
	((and (boundwff-q wff1)
	      (boundwff-q wff2)
	      (equal (binding wff1) (binding wff2))
	      (equal (bindvar wff1) (bindvar wff2)))
	 (cons (car wff1)
	       (subst-occs-template x term1 (cdr wff1) term2 (cdr wff2))))
	((or (boundwff-q wff1)
	     (boundwff-q wff2))
	 (throwfail "Illegal application of subst="))
	(t (cons (subst-occs-template x term1 (car wff1) term2 (car wff2))
		 (subst-occs-template x term1 (cdr wff1) term2 (cdr wff2))))))

; returns values (P0 . . . Pn) ((A1 . B1) . . . (An . Bn)) <justs>
; where Pi follows from P(i-1) using Ai = Bi
; and each Ai = Bi follows from Ai = smallterm and Bi = smallterm
; where smallterm is the "smaller" of term1 and term2 (one is a proper subterm
; of the other)
(defun subst-occs-iterations (wff1 wff2 term1 term2)
  (let* ((len-limit (max (wff-length wff1) (wff-length wff2)))
	 (eq-list-w (generate-eq-list-with-justifications
		     term1 term2 len-limit)))
    (multiple-value-bind
     (eq-used justs)
     (subst-occs-eq-used-and-justifications
      wff1 wff2 eq-list-w)
     (values
      (subst-occs-iterations*
       wff1 wff2 eq-used)
      eq-used
      justs))))

; returns values: list of (<a> . <b>) (where |- a = b and this equation is used to go from wff1 to wff2)
;             and list of (<a> . . . <x>) for each a on one side of an equation above.
(defun subst-occs-eq-used-and-justifications (wff1 wff2 eq-list-w)
  (cond ((wffeq wff1 wff2)
	 (values nil nil))
	((label-q wff1) (apply-label wff1
				     (subst-occs-eq-used-and-justifications wff1 wff2 eq-list-w)))
	((label-q wff2) (apply-label wff2
				     (subst-occs-eq-used-and-justifications wff1 wff2 eq-list-w)))
	((and (member wff1 eq-list-w :test #'(lambda (x y) (wffeq x (car y))))
	      (member wff2 eq-list-w :test #'(lambda (x y) (wffeq x (car y)))))
	 (let ((wff1-w (car (member wff1 eq-list-w :test #'(lambda (x y) (wffeq x (car y))))))
	       (wff2-w (car (member wff2 eq-list-w :test #'(lambda (x y) (wffeq x (car y)))))))
	   (values (list (cons wff1 wff2))
		   (list (cdr wff1-w) (cdr wff2-w)))))
	((or (lsymbol-q wff1) (lsymbol-q wff2))
	 (throwfail "Bad application of Subst="))
	((and (boundwff-q wff1)
	      (boundwff-q wff2))
	 (subst-occs-eq-used-and-justifications (cdr wff1) (cdr wff2) eq-list-w))
	((or (boundwff-q wff1)
	     (boundwff-q wff2))
	 (throwfail "Bad application of Subst="))
	(t 
	 (multiple-value-bind
	  (eq-used1 justs1)
	  (subst-occs-eq-used-and-justifications (car wff1) (car wff2) eq-list-w)
	  (multiple-value-bind
	   (eq-used2 justs2)
	   (subst-occs-eq-used-and-justifications (cdr wff1) (cdr wff2) eq-list-w)
	   (values (union eq-used1 eq-used2 :test #'(lambda (x y)
						      (and (wffeq (car x) (car y))
							   (wffeq (cdr x) (cdr y)))))
		   (union justs1 justs2 :test #'(lambda (x y)
						  (wffeq (car x) (car y))))))))))
	 

; returns list of <wff>'s
; where eq-used is list of (<ai> . <bi>) and we go from wff_i to wff_(i+1) using <ai> = <bi>
; and wff1 is wff_1 and wff2 is wff_(n+1) where n is the length of eq-used
(defun subst-occs-iterations* (wff1 wff2 eq-used)
  (if eq-used
      (let ((wff (perform-subst-occs (caar eq-used) (cdar eq-used) wff1 wff2)))
	(cons wff1
	      (subst-occs-iterations* wff wff2 (cdr eq-used))))
    (if (wffeq wff1 wff2)
	(list wff2)
      (throwfail "Expected " (wff1 . gwff) " to equal " (wff2 . gwff)))))
	    
; replace term1 with term2 in wff1 where term1 occurs in wff1 and term2 occurs
; in wff2
(defun perform-subst-occs (term1 term2 wff1 wff2)
  (cond ((wffeq wff1 wff2)
	 wff1)
	((and (wffeq term1 wff1)
	      (wffeq term2 wff2))
	 term2)
	((label-q wff1)
	 (apply-label wff1
		      (perform-subst-occs term1 term2 wff1 wff2)))
	((label-q wff2)
	 (apply-label wff2
		      (perform-subst-occs term1 term2 wff1 wff2)))
	((or (lsymbol-q wff1) (lsymbol-q wff2))
	 wff1)
	((and (boundwff-q wff1)
	      (boundwff-q wff2))  ; assumes binders and bindvars are the same
	 (cons (car wff1)
	       (perform-subst-occs term1 term2 (cdr wff1) (cdr wff2))))
	((or (boundwff-q wff1)
	     (boundwff-q wff2))
	 wff1)
	(t (cons (perform-subst-occs term1 term2 (car wff1) (car wff2))
		 (perform-subst-occs term1 term2 (cdr wff1) (cdr wff2))))))

; Compare to generate-eq-list in wffsub1.lisp,
; but this code is completely different.
; We could simplify generate-eq-list using an
; algorithm such as this one, except without justifications
; and without assuming one term is a subterm of the other.
(defun generate-eq-list-with-justifications (term1 term2 len-limit)
  (labels ((carwffeq (wff-just1 wff-just2)
		     (wffeq (car wff-just1) (car wff-just2))))
	  (let* ((bigterm (if (cdr (substitute-term-term term2 term1 term2))
			      term2
			    term1))
		 (smallterm (if (eq bigterm term2) term1 term2)))
	    (do* ((old-terms (list (cons smallterm (list smallterm)))
			     (nconc old-terms new-terms))
		  (new-terms (list (cons bigterm (list bigterm smallterm)))
			     (set-difference
			      (delete-duplicates
			       (delete-if
				#'(lambda (x)
				    (> (wff-length (car x)) len-limit))
				(mapcan #'(lambda (x)
					    (mapcar #'(lambda (y)
							(cons y
							      (cons y (cdr x))))
						    (cdr (substitute-term-term
							  bigterm smallterm (car x)))))
					new-terms))
			       :test #'carwffeq)
			      old-terms
			      :test #'carwffeq)))
		 ((null new-terms) old-terms)))))

; Rewrote this so that the sym= line is replaced by
; a small derivation using Leibniz equality.
; This has a chance of being normal.  (This Sym= derivation
; corresponds to a derived rule taking one extraction premiss
; and returning a normal conclusion.) - cebrown 1/22/00
(defun remove-sym=-line (line)
  (let* ((ln (linealias line))
	 (linejust (car (caddr (line-justification line))))
	 (eqn (line-assertion line))
	 (l (cdar eqn))
	 (q (fresh-var (cons 'O (type l)) '\q))
	 (x (fresh-var-1 (type l)))
	 (qsub (cons (cons x 'LAMBDA)
		     (cons 'NOT (cons q x))))
	 (REWRITE-EQUALITIES 'LEIBNIZ))
    (declare (special REWRITE-EQUALITIES))
    (introduce-gap line 8)
    (comdecode (list 'cl-user::plan (+ ln 8)))
    (comdecode (list 'cl-user::equiv-eq-expd (linealias linejust) ln '$ '$ '$ '$))
    (comdecode (list 'cl-user::lcontr* ln (+ ln 1) '$ '$ '$ '$))
    (comdecode (list 'cl-user::ui (+ ln 1) (+ ln 2) (list 'quote qsub)
		     '$ '$ '$ '$ '$))
    (comdecode (list 'cl-user::lcontr* (+ ln 2) (+ ln 3) '$ '$ '$ '$))
    (comdecode (list 'cl-user::equiv-eq-contr (+ ln 8) (+ ln 7) '$ '$ '$ '$))
    (comdecode (list 'cl-user::lexpd* (+ ln 7) (+ ln 6) '$ '$ '$ '$))
    (comdecode (list 'cl-user::abu (+ ln 6) (+ ln 5) (list 'quote q) '$ '$ '$ '$ '$))
    (comdecode (list 'cl-user::ugen (+ ln 5) (+ ln 4) '$ (list 'quote q)))
    (comdecode (list 'cl-user::rulep (+ ln 4) (list (+ ln 3))))))

(defun remove-assert-sym=-line (line)
  (let* ((ln (linealias line))
	 (sym (line-assertion line))
	 (e1 (cdar sym))
	 (e2 (cdr sym))
	 (rewrite-equalities 'eager)
	 (ei1 (instantiate-top-equality e1))
	 (ei2 (instantiate-top-equality e2))
	 (eil1 (lcontr (cons (lcontr (car ei1)) (cdr ei1))))
	 (eil2 (lcontr (cons (lcontr (car ei2)) (cdr ei2))))
	 (x (create-propsym '\x (cdr (unabbreviated-type (caar eil2))))))
    (introduce-gap line 6)
    (setf (line-hypotheses line) nil) ; assert needs no hyps
    (comdecode (list 'cl-user::plan (+ ln 6)))
    (comdecode (list 'cl-user::lemma (+ ln 6) (+ ln 5)
		     (list 'quote (cons (cons 'IMPLIES eil1) eil2))
		     '$ '$ '$))
    (comdecode (list 'cl-user::equiv-eq (+ ln 5) (+ ln 6) '$ '$ '$ '$))
    (comdecode (list 'cl-user::deduct (+ ln 5) (+ ln 4) ln '$ '$ '$ '$ '$))
    (comdecode (list 'cl-user::lemma (+ ln 4) (+ ln 3) 
		     (list 'quote (cdr eil2)) '$ '$ '$))
    (comdecode (list 'cl-user::ugen (+ ln 4) (+ ln 3) '$ (list 'quote (caar eil2))))
    (comdecode (list 'cl-user::lemma (+ ln 3) (+ ln 2)
		     (list 'quote
			   (cons (cons 'IMPLIES (cons 'NOT (cddr eil2)))
				 (cons 'NOT (cdadr eil2))) '$ '$ '$)))
    (comdecode (list 'cl-user::rulep (+ ln 3) (list 'quote (list (+ ln 2)))))
    (comdecode (list 'cl-user::lemma (+ ln 2) (+ ln 1)
		     (list 'quote
			   (lcontr (cons (cons (cons (caar eil1) 'LAMBDA)
					       (cdr eil1))
					 (cons (cons x 'LAMBDA)
					       (cons 'NOT (cons (caar eil2) x))))))))
    (comdecode (list 'cl-user::lambda* (+ ln 1) (+ ln 2) '$ '$ '$ '$))
    (comdecode (list 'cl-user::ui ln (+ ln 1)
		     (list 'quote 
			   (cons (cons x 'LAMBDA)
				 (cons 'NOT 
				       (cons (caar eil2) 
					     x))))
		     '$ '$ '$ '$ '$))))


(defun get-rewrite-defns (lines)
  (let* ((deflines (remove-if-not #'(lambda (x) (member (car (line-justification x))
							'("Defn" "EquivWffs")
							:test 'string=))
				  lines))
	 (rewrite-defns '(EAGER))
	 (defns nil)
	 (inst nil)
	 (uninst nil))
    (dolist (line deflines)
	    (let* ((ass1 (get line 'assertion))
		   (ass2 (get (caaddr (get line 'justification)) 'assertion))
		   (*instantiated* nil)
		   (def1 (mapcar 'getabbr (abbr-list ass1)))
		   (def2 (mapcar 'getabbr (abbr-list ass2))))
	      (declare (special *instantiated*))
	      (wffeq-def1b ass1 ass2 nil nil)
	      (push (mapcar 'getabbr *instantiated*) inst)
	      (push def1 defns)
	      (push def2 defns)))
    (setq defns (delete 'equiv (remove-duplicates (apply 'append defns))))
    (setq inst (delete 'equiv (remove-duplicates (apply 'append inst))))
    (setq uninst (setdiff defns inst))
    ;idea: a defn appears at some point on a branch. If on some branch it never disappears, or disappears at a
    ;DEF or EQUIVWFFS without being instantiated, then we need LAZY2 for it. o/w use EAGER.
    (let ((*return* nil)
	  (abbrs (remove-duplicates (mapcar 'getabbr (abbr-list (get (car (last lines)) 'assertion))))))
      (declare (special *return*))
      (uninst-branch (car (last lines)) abbrs)
      (list (remove-duplicates (remove-if #'(lambda (x) (or (eq x 'EQUIV) (memq x uninst))) *return*))
	    (remove-if #'(lambda (x) (eq x 'EQUIV)) uninst))
)))

(defun getabbr (a)
  (or (get a 'core::stands-for) a))

(defun uninst-branch (line abbrs)
  (declare (special *return*))
;  (msgf (linealias line) " with abbrs " abbrs)
  (let ((just (line-justification line)))
    (if (null (caddr just))
	(setq *return* (remove-duplicates (append abbrs *return*))) ;nil
      (if (member (car just) '("Defn" "EquivWffs") :test 'string=)
	  (let* ((*instantiated* nil)
		 (ass1 (get line 'assertion))
		 (ass2 (get (caaddr just) 'assertion))
		 (def1 (mapcar 'getabbr (abbr-list ass1)))
		 (def2 (mapcar 'getabbr (abbr-list ass2)))
		 (diff (union (setdiff def2 def1) (setdiff def1 def2)))
		 (exceptions nil))
	    (wffeq-def1b ass1 ass2 nil nil)
	    (setq *instantiated* (mapcar 'getabbr *instantiated*))
	    (dolist (i *instantiated*)
		    (when (and (memq i def1) (memq i def2)
			       (not (= (length (remove-if-not #'(lambda (x) (eq x i)) def1))
				       (length (remove-if-not #'(lambda (x) (eq x i)) def2)))))
			    ;if some i's were instantiated and some weren't
			    (push i exceptions)))
;	    (msgf "I: " *instantiated* t "E: " exceptions)
	    (dolist (i *instantiated*)
		    (setq abbrs (remove-if #'(lambda (x) (eq x i)) abbrs))
		    (setq def1 (remove-if #'(lambda (x) (eq x i)) def1))
		    (setq def2 (remove-if #'(lambda (x) (eq x i)) def2)))
	    (dolist (d diff)
		    (when (memq d abbrs) 
;			  (msgf "Adding " d t)
			  (push d *return*)))
	    (uninst-branch (caaddr just) (remove-duplicates (append exceptions (append def1 (append def2 abbrs))))))
	(let ((useful-jlines (if (string= (car just) "RuleC") (cdaddr just) (caddr just))))
	(dolist (j useful-jlines)
		(let ((newabbs (union (mapcar 'getabbr (abbr-list (line-assertion line)))
				      (mapcar 'getabbr (abbr-list (line-assertion j))))))
		  (uninst-branch j (remove-duplicates (intersection newabbs
								    abbrs))))))))))

(defun wffeq-def1b (wff1 wff2 varstack switch)
  (declare (special *instantiated* min-quant-etree)) ;this is the only difference between wffeq-def1b and wffeq-def1
  (let ((lsym-1 (lsymbol-q wff1))
	(lsym-2 (lsymbol-q wff2))
	(bdwff-1 (boundwff-q wff1))
	(bdwff-2 (boundwff-q wff2)))
    (cond ((label-q wff1)
	   (apply-label wff1 (wffeq-def1b wff1 wff2 varstack switch)))
	  ((label-q wff2)
	   (apply-label wff2 (wffeq-def1b wff1 wff2 varstack switch)))
	  ((and lsym-1 lsym-2)
	   (cond ((not (type-equal wff1 wff2)) nil) ; Saves lots of work
		 ((or (anypropsym-q wff1) (anypropsym-q wff2))
		  (if switch
		      (true-correspondence wff2 wff1 varstack)
		      (true-correspondence wff1 wff2 varstack)))
		 ((equal wff1 wff2))
		 ((anyabbrev-q wff1) 
		  (and (wffeq-def1b wff2 (get-defn-1 wff1) varstack (not switch))
		       (push wff1 *instantiated*)))
		 ((anyabbrev-q wff2) 
		  (and (wffeq-def1b (get-defn-1 wff2) wff1 varstack (not switch))
		       (push wff2 *instantiated*)))
		 (t nil)))
	  ((and bdwff-1 bdwff-2)
	   (or (and (eq (binding wff1) (binding wff2))
		    (type-equal (bindvar wff1) (bindvar wff2))
		    (wffeq-def1b
		     (gdr wff1) (gdr wff2)
		     (cons (if switch
			       (cons (bindvar wff2) (bindvar wff1))
			       (cons (bindvar wff1) (bindvar wff2)))
			   varstack) switch))
	       (and (anyabbrev-q (binding wff1))
		    (wffeq-def1b 
		     wff2 (get-def-binder1 wff1) varstack (not switch))
		    (push (binding wff1) *instantiated*))
	       (and (anyabbrev-q (binding wff2))
		    (wffeq-def1b 
		     (get-def-binder1 wff2) wff1 varstack (not switch))
		    (push (binding wff2) *instantiated*))))
	  ((and lsym-1 bdwff-2)
	   (wffeq-def-lsym-bind-chk wff1 wff2 varstack switch))
	  ((and bdwff-1 lsym-2)
	   (wffeq-def-lsym-bind-chk wff2 wff1 varstack (not switch)))
	  ((and (not bdwff-1) (not bdwff-2) (not lsym-1) (not lsym-2)
		(wffeq-def1b (gdr wff1) (gdr wff2) varstack switch)
		(wffeq-def1b (gar wff1) (gar wff2) varstack switch)))
	  (t
	   (let* ((igar1 (inmost-gar wff1))
		  (igar2 (inmost-gar wff2))
		  (lambda-i-1 (lambda-bd-p igar1))
		  (bdwff-i-1 (boundwff-q igar1))
		  (anyabb-i-1 (anyabbrev-p igar1)))
	     (cond ((or (and (not bdwff-1) lambda-i-1)
			(and (not bdwff-2) (lambda-bd-p igar2)))
		    (if (not lambda-i-1)
			(wffeq-def1b (lambda-norm wff2) wff1 varstack
				    (not switch))
			(wffeq-def1b (lambda-norm wff1) wff2 varstack switch)))
		   ((or anyabb-i-1 (anyabbrev-p igar2))
		    (if (and (module-loaded-p 'auto-basic) min-quant-etree)
			(if (not anyabb-i-1)
			    (and (wffeq-def1b (lnorm (instantiate-1 wff2)) wff1 varstack
					      (not switch))
				 (push (car (abbr-list wff2)) *instantiated*)) ;OK since abbr-list also gets inmost-gar
			  (and (wffeq-def1b (lnorm (instantiate-1 wff1)) wff2 varstack
					    switch)
			       (push (car (abbr-list wff1)) *instantiated*)))
		      (if (not anyabb-i-1)
			  (and (wffeq-def1b (instantiate-1 wff2) wff1 varstack
					(not switch))
			       (push (car (abbr-list wff2)) *instantiated*))
			(and (wffeq-def1b (instantiate-1 wff1) wff2 varstack
					  switch)
			     (push (car (abbr-list wff1)) *instantiated*)))))
		   ((or (and bdwff-i-1
			     (anyabbrev-q (binding igar1)))
			(and (boundwff-q igar2)
			     (anyabbrev-q (binding igar2))))
		    (if (and (module-loaded-p 'auto-basic) min-quant-etree)
			(if (or (not bdwff-i-1)
				(not (anyabbrev-q (binding igar1))))
			    (and (wffeq-def1b (lnorm (instantiate-1 wff2)) wff1 varstack
					      (not switch))
				 (push (car (abbr-list wff2)) *instantiated*))
			  (and (wffeq-def1b (lnorm (instantiate-1 wff1)) wff2 varstack
					    switch)
			       (push (car (abbr-list wff1)) *instantiated*)))
		      (if (or (not bdwff-i-1)
			      (not (anyabbrev-q (binding igar1))))
			  (and (wffeq-def1b (instantiate-1 wff2) wff1 varstack
					    (not switch))
			       (push (car (abbr-list wff2)) *instantiated*))
			(and (wffeq-def1b (instantiate-1 wff1) wff2 varstack
					  switch)
			     (push (car (abbr-list wff1)) *instantiated*)))))
		   (t nil)))))))

; unused
(defun wffeq-def-lsym-bind-chk-b (lsym bind varstack switch)
  (declare (special *instantiated*))
  (cond ((anyabbrev-q lsym)
	 (and (wffeq-def1b bind (get-defn-1 lsym) varstack (not switch))
	      (push lsym *instantiated*)))
	((anyabbrev-q (binding bind))
	 (and (wffeq-def1b (get-def-binder1 bind) lsym varstack (not switch))
	      (push bind *instantiated*)))
	(t nil)))
