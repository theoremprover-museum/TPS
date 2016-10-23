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

(defun display-dup-info (dup-info)
  (declare (special natree-debug))
  (if natree-debug
      (let ((*ignore-statuses* T))
	(display-vp-diag (etree-to-jform dup-info))
	(display-etree-all dup-info))))

;;;dup-info is actually expansion tree

(defun negate-etree (etree)
  (setf (etree-positive etree) (not (etree-positive etree)))
  (case (etree-junctive etree)
	(CON (setf (etree-junctive etree) 'DIS))
	(DIS (setf (etree-junctive etree) 'CON)))
  (mapc #'negate-etree (etree-components etree))
  etree)

(defvar otherwise T)

;;;dup-info should really be an expansion tree without mating!!!

(defun print-dup-info-debug (dup-info)
  (display-etree-all-main dup-info))

(defun copy-dup-info (dup-info)
;  (describe (first (etree-components dup-info)))
  (copy-etree-rec dup-info T))

(defun empty-dup-info (natree pos)
  (make-empty-dup-info :shallow (natree-assertion natree)
		       :positive pos
		       :components nil
		       :parent nil
		       :junctive nil))

(defun empty-dup-info-1 (assertion pos)
  (make-empty-dup-info :shallow assertion
		       :positive pos
		       :components nil
		       :parent nil
		       :junctive nil))

(defun selection-dup-info (shallow jterm son pos)
  (let ((parent (make-selection :components (list son)
				:positive pos
				:junctive 'NEUTRAL
				:free-vars nil
				:parent nil
				:shallow shallow
				:terms jterm)))
    (setf (etree-parent son) parent) parent))

(defun skolem-dup-info (natree son pos)
  (let ((parent (make-skolem :components (list son)
			     :positive pos
			     :junctive 'NEUTRAL
			     :free-vars nil
			     :parent nil
			     :shallow (natree-assertion natree)
			     :terms (natree-jterm natree))))
    (setf (etree-parent son) parent) parent))

(defun expansion-dup-info (shallow jterm son pos)
  (let ((parent (make-expansion :components (list son)
				:positive pos
				:junctive 'CON
				:free-vars NIL
				:parent nil
				:shallow shallow
				:terms jterm)))
    (setf (etree-parent son) parent) parent))

(defun negation-dup-info (son pos)
  (let ((parent (make-negation :components (list son)
			       :positive pos
			       :junctive 'NEUTRAL
			       :free-vars nil
			       :parent nil)))
    (setf (etree-parent son) parent) parent))

(defun econjunction-dup-info (lson rson pos)
  (let ((parent (make-econjunction :components (list lson rson)
				   :positive pos
				   :junctive (if pos 'CON 'DIS)
				   :free-vars nil
				   :parent nil)))
    (setf (etree-parent lson) parent
	  (etree-parent rson) parent)))

(defun edisjunction-dup-info (lson rson pos)
  (let ((parent (make-edisjunction :components (list lson rson)
				   :positive pos
				   :junctive (if pos 'DIS 'CON)
				   :free-vars nil
				   :parent nil)))
    (setf (etree-parent lson) parent
	  (etree-parent rson) parent)))

(defun implication-dup-info (lson rson pos)
  (let ((parent (make-implication :components (list lson rson)
				  :positive pos
				  :junctive (if pos 'DIS 'CON)
				  :free-vars nil
				  :parent nil)))
    (setf (etree-parent lson) parent
	  (etree-parent rson) parent)))

(defun hd-substitute-in-natree (term hdvar natree)
  (substitute-in-etree-main term hdvar (natree-dup-info-neg natree))
  (dolist (hyp (natree-hypo natree))
	  (substitute-in-etree-main term hdvar (natree-dup-info-pos hyp))))

(defun update-dup-info (dup-info)
  (cond ((leaf-p dup-info) (deepen-dup-info dup-info))
	((rewrite-p dup-info)
	 (update-dup-info (first (etree-components dup-info))))
	(otherwise dup-info)))

(defun update-selection-dup-info (dup-info)
  (cond ((leaf-p dup-info) (deepen-dup-info dup-info))
	((rewrite-p dup-info)
	 (update-dup-info (first (etree-components dup-info))))
	((selection-p dup-info)
	 (if (etree-leaf dup-info)
	     (let ((newterm (make-skolem-fn (bindvar (selection-shallow dup-info)) nil)))
	       (substitute-in-etree-main newterm 
					 (first (selection-terms dup-info))  
					 dup-info)))
	 dup-info)
	(otherwise (describe dup-info)
		   (throwfail "Unexpected occurred in update-selection-dup-info!"))))

(defun update-expansion-dup-info (dup-info var)
  (cond ((leaf-p dup-info)
	 (let ((new-dup-info (deepen-dup-info dup-info)))
	   (substitute-in-etree-main var (first (expansion-terms new-dup-info))  new-dup-info)))
	((rewrite-p dup-info) (update-expansion-dup-info (first (etree-components dup-info)) var))
	((expansion-p dup-info)
	 (substitute-in-etree-main var (first (expansion-terms dup-info))  dup-info))
	(otherwise (describe dup-info)
		   (throwfail "Unexpected occurred in update-expansion-dup-info!"))))


(defun attach-dup-info-to-natree (natree dup-info)
; Comment the next two lines unless debugging
  (msgf "I am working on " (natree-justification natree))
  (msgf "The assertion is " ((natree-assertion natree) . gwff))
  (if (empty-dup-info-p dup-info)
      (progn (msgf "Skipping an empty-dup-info!")
	     (unless (natree-dup-info-neg natree)
		     (setf (natree-dup-info-neg natree) (empty-dup-info natree nil))))
  (let ((support (natree-support natree)))
    (ecase (natree-justification natree)
       (UI (let ((premiss (first support))
		 (parent (expansion-dup-info (natree-assertion (first support))
					     (natree-jterm natree) dup-info T)))
	     (attach-dup-info-to-natree premiss parent)
	     (let ((new-dup-info (update-selection-dup-info (natree-dup-info-neg premiss))))
;;;	       (describe new-dup-info)
	       (setf (natree-dup-info-neg natree)
		     (copy-dup-info (first (etree-components new-dup-info))))
	       (hd-substitute-in-natree (first (natree-jterm natree))
					(first (selection-terms new-dup-info)) natree))))
       (UGEN (setq dup-info
		   (update-expansion-dup-info dup-info
					      (first (natree-jterm natree))))
	     (let ((premiss (first support))
		   (var (first (natree-jterm natree)))
		   (terms (expansion-terms dup-info)))
;;;This is really a bugged version; I will fix this someday.
	       (attach-dup-info-to-natree premiss (first (etree-components dup-info)))
	       (let ((new-dup-info (empty-dup-info natree nil))
		     (etree (natree-dup-info-neg premiss)))
		 (dolist (term terms (setf (natree-dup-info-neg premiss) new-dup-info))
			 (setq new-dup-info
			       (merge-dup-info new-dup-info 
					       (substitute-in-etree-main term var (copy-dup-info etree))))))
	       (dolist (hyp (natree-hypo premiss))
		       (let ((new-dup-info (empty-dup-info hyp T))
			     (etree (natree-dup-info-pos hyp)))
			 (dolist (term terms (setf (natree-dup-info-pos hyp) new-dup-info))
				 (setq new-dup-info
				       (merge-dup-info new-dup-info 
						       (substitute-in-etree-main term var (copy-dup-info etree)))))))
	       (setf (natree-dup-info-neg natree)
		     (selection-dup-info (natree-assertion natree) (natree-jterm natree)
					 (natree-dup-info-neg premiss) nil))))
       (CHOOSE (let ((premiss (first support)))
		 (attach-dup-info-to-natree premiss
					    (selection-dup-info (natree-assertion premiss)
								(natree-jterm natree) dup-info T))))
       (RULEC (let ((premiss (first support))
		    (spremiss (second support))
		    (choose (first (set-difference (natree-hypo (second support)) (natree-hypo natree)))))
		(when choose                                              ;;; "choose" may have been deleted by clean-rulec!
		      (setf (natree-dup-info-pos choose) (empty-dup-info natree T))
		      (setf (natree-dup-info-neg choose)
			    (initialize-dup-info (natree-assertion choose) nil))
		      (setf (natree-support choose) (list premiss)))
		(attach-dup-info-to-natree spremiss dup-info)
		(setf (natree-dup-info-neg natree) (natree-dup-info-neg spremiss))
		(if choose
		    (let* ((var (first (natree-jterm choose)))
			   (pdup-info (update-expansion-dup-info (natree-dup-info-neg premiss) var))
			   (terms (if (not (empty-dup-info-p pdup-info)) (expansion-terms pdup-info))))
		      (dolist (hyp (natree-hypo natree))
			      (let ((new-dup-info (empty-dup-info hyp T))
				    (etree (natree-dup-info-pos hyp)))
				(dolist (term terms (setf (natree-dup-info-pos hyp) new-dup-info))
					(setq new-dup-info
					      (merge-dup-info new-dup-info 
							      (substitute-in-etree-main term var (copy-dup-info etree)))))))
		      (let ((new-dup-info (empty-dup-info natree nil))
			    (etree (natree-dup-info-neg natree)))
			(dolist (term terms (setf (natree-dup-info-neg natree) new-dup-info))
				(setq new-dup-info
				      (merge-dup-info new-dup-info 
						      (substitute-in-etree-main term var (copy-dup-info etree))))))))))
;;;there is a bug here: what happens if ""(first (natree-jterm natree))"
;;;is also a bound variable in (selection-shallow dup-info)?
;;;this really is a deep bug in substitute-in-etree-main since
;;;it takes "free-for" for granted.
       (EGEN (setq dup-info (update-dup-info dup-info))
	     (let ((premiss (first support))
		   (new-dup-info (substitute-in-etree-main (first (natree-jterm natree))
							   (first (selection-terms dup-info))
							   (first (etree-components dup-info)))))
	       (attach-dup-info-to-natree premiss new-dup-info)
	       (setf (natree-dup-info-neg natree)
		     (expansion-dup-info (natree-assertion natree) (natree-jterm natree)
					 (natree-dup-info-neg premiss) nil))))
       ((AB |SAME AS|)
	(let ((premiss (first support)))
	  (attach-dup-info-to-natree premiss dup-info)
	  (setf (natree-dup-info-neg natree) (natree-dup-info-neg premiss))))
       (ABSURD (attach-dup-info-to-natree (first support)
					  (initialize-dup-info (natree-assertion (first support)) T))
	       (setf (natree-dup-info-neg natree)
		     (initialize-dup-info (natree-assertion natree) nil)))
       (|ASSERT DESCR|
	(setf (natree-dup-info-neg natree)
	      (initialize-dup-info (natree-assertion natree) nil)))
       ((|ASSERT REFL=| REFL=) ; included REFL= here - cebrown 8/30/99
; I changed the way Reflexivity is handled.  - cebrown 8/30/99
;	(setf (natree-dup-info-neg natree)
;	      (make-true :positive nil
;			 :junctive 'DIS
;			 :parent nil
;			 :components nil))
	(setf (natree-dup-info-neg natree) ; rewrite node from = to TRUTH
	      (dup-info-rewrite-exp-ctr nil 'REFL=
					(make-true :positive nil
						   :junctive 'DIS
						   :parent nil
						   :components nil)
					(initialize-dup-info (natree-assertion natree) nil))))
       ((|ASSUME NEGATION| HYP)
	(setf (natree-dup-info-pos natree)
	      (merge-dup-info (natree-dup-info-pos natree) dup-info)))
       (|CASE 1| 
	(let ((empty-dup-info (empty-dup-info-1 (cdr (natree-assertion (first support))) T)))
	  (setf (natree-dup-info-pos natree) empty-dup-info)
	  (attach-dup-info-to-natree (first support) 
				     (edisjunction-dup-info dup-info empty-dup-info T)))
;;;	(xdisplay-dup-info (natree-dup-info-neg (first support)))
	(setf (natree-dup-info-neg natree)
	      (first (etree-components (update-dup-info (natree-dup-info-neg (first support)))))))
       (|CASE 2|
	(let ((empty-dup-info (empty-dup-info-1 (cdar (natree-assertion (first support))) T)))
	  (setf (natree-dup-info-pos natree) empty-dup-info)
	  (attach-dup-info-to-natree (first support) 
				     (edisjunction-dup-info empty-dup-info dup-info T)))
;;;	(xdisplay-dup-info (natree-dup-info-neg (first support)))
	(setf (natree-dup-info-neg natree)
	      (second (etree-components (update-dup-info (natree-dup-info-neg (first support)))))))
       (CASES (let ((co-dup-info (copy-dup-info dup-info)))
		(attach-dup-info-to-natree (second support) dup-info)
		(attach-dup-info-to-natree (third support) co-dup-info)
		(setf (natree-dup-info-neg natree)
		      (merge-dup-info (natree-dup-info-neg (second support))
				      (natree-dup-info-neg (third support))))))
       (CONJ (cond ((= 2 (length support))
		    (setq dup-info (update-dup-info dup-info))
		    (let ((lpremiss (first support))
			  (rpremiss (second support)))
		      (attach-dup-info-to-natree lpremiss (first (etree-components dup-info)))
		      (attach-dup-info-to-natree rpremiss (second (etree-components dup-info)))
		      (setf (natree-dup-info-neg natree)
			    (econjunction-dup-info (natree-dup-info-neg lpremiss) (natree-dup-info-neg rpremiss) NIL))))
		   ((= 1 (length support))
		    (let ((premiss (first support)))
		      (cond ((wffeq (natree-assertion natree)
				    (cdar (natree-assertion premiss)))
			     (attach-dup-info-to-natree premiss
							(econjunction-dup-info dup-info 
									       (empty-dup-info-1 (cdr (natree-assertion premiss)) T) T))
			     (setf (natree-dup-info-neg natree)
				   (first (etree-components (update-dup-info (natree-dup-info-neg premiss))))))
			     ((wffeq (natree-assertion natree)
				     (cdr (natree-assertion premiss)))
			      (attach-dup-info-to-natree premiss 
							 (econjunction-dup-info (empty-dup-info-1 (cdar (natree-assertion premiss)) T)
										dup-info T))
			      (setf (natree-dup-info-neg natree)
				    (second (etree-components (update-dup-info (natree-dup-info-neg premiss))))))
			     (otherwise (throwfail (msg ((natree-assertion natree) . gwff)
							" is not a branch in " 
							 ((natree-assertion premiss) . gwff)))))))))
       (DEDUCT (setq dup-info (update-dup-info dup-info))
	       (let* ((premiss (first support))
		      (hypo (first (set-difference (natree-hypo premiss)
						   (natree-hypo natree)))))
		 ;;; make sure hypo is used.
		 (if hypo (setf (natree-dup-info-pos hypo) (empty-dup-info hypo T)
				(natree-dup-info-neg hypo)
				(merge-dup-info-lenient (natree-dup-info-neg hypo)
							(first (etree-components dup-info)))))
		 (attach-dup-info-to-natree premiss (second (etree-components dup-info)))
		 (setf (natree-dup-info-neg natree)
		       (implication-dup-info (if hypo (natree-dup-info-pos hypo)
					       (empty-dup-info-1 (cdar (natree-assertion natree)) T))
					     (natree-dup-info-neg premiss) NIL))))
       (INDIRECT (let* ((premiss (first support))
			(spremiss (second support))
			(hypo (first (set-difference (natree-hypo premiss) (natree-hypo natree)))))
		   (if hypo (setf (natree-dup-info-pos hypo)
				  (empty-dup-info hypo T)
				  (natree-dup-info-neg hypo)
				  (neg-dup-info-with-assertion (deepen-dup-info* dup-info)
							       (natree-assertion hypo) T)))
		   (attach-dup-info-to-natree premiss (initialize-dup-info (natree-assertion premiss) T))
		   (if spremiss
		       (attach-dup-info-to-natree spremiss
						  (neg-dup-info-with-assertion (natree-dup-info-neg premiss)
									       (natree-assertion spremiss) T)))
		   (if hypo (setf (natree-dup-info-neg natree)
				  (neg-dup-info-with-assertion (deepen-dup-info* (natree-dup-info-pos hypo))
							       (natree-assertion natree) T))
		            (setf (natree-dup-info-neg natree) (empty-dup-info natree nil)))))
       (MP (let ((lpremiss (first support))
		 (rpremiss (second support)))
	     (attach-dup-info-to-natree lpremiss (initialize-dup-info (natree-assertion lpremiss) T))
;;;	     (xdisplay-dup-info (natree-dup-info-neg lpremiss))
	     (attach-dup-info-to-natree rpremiss (implication-dup-info (natree-dup-info-neg lpremiss) dup-info T))
;;;	     (xdisplay-dup-info (natree-dup-info-neg lpremiss))
	     (upward-update-natree lpremiss (first (etree-components (update-dup-info (natree-dup-info-neg rpremiss)))))
	     (setf (natree-dup-info-neg natree)
		   (second (etree-components (update-dup-info (natree-dup-info-neg rpremiss)))))))
       ((NEG DISJ-IMP IMP-DISJ)
       (let ((premiss (first support)))
	 (setq dup-info (deepen-dup-info* dup-info))
	 (attach-dup-info-to-natree premiss
				    (neg-dup-info-with-assertion dup-info (natree-assertion premiss) T))
	 (setf (natree-dup-info-neg natree)
	       (neg-dup-info-with-assertion (deepen-dup-info* (natree-dup-info-neg premiss))
					    (natree-assertion natree) T))))
       (NEGINTRO (let* ((premiss (first support))
			(hypo (first (set-difference (natree-hypo premiss)
						     (natree-hypo natree)))))
		   (setf (natree-dup-info-pos hypo) (empty-dup-info hypo T)
			 (natree-dup-info-neg hypo) (first (etree-components (update-dup-info dup-info))))
		   (attach-dup-info-to-natree premiss (initialize-dup-info (natree-assertion premiss) T))
		   (setf (natree-dup-info-neg natree)
			 (neg-dup-info-with-assertion (natree-dup-info-pos hypo)
						      (natree-assertion natree) T))))
       (NEGELIM (let* ((lpremiss (first support))
		       (rpremiss (second support)))
		  (attach-dup-info-to-natree lpremiss (initialize-dup-info (natree-assertion lpremiss) T))
		  (attach-dup-info-to-natree rpremiss (initialize-dup-info (natree-assertion rpremiss) T))
		  (attach-dup-info-to-natree lpremiss
					     (neg-dup-info-with-assertion (natree-dup-info-neg rpremiss)
									  (natree-assertion lpremiss) T))
		  (attach-dup-info-to-natree rpremiss
					     (neg-dup-info-with-assertion (natree-dup-info-neg lpremiss)
									  (natree-assertion rpremiss) T))))
       (IMPEQUIV (let ((premiss (first support)))
		   (attach-dup-info-to-natree premiss (update-dup-info dup-info))
		   (let* ((son (natree-dup-info-neg premiss))
			  (new-dup-info (make-rewrite :components (list son)
						      :positive nil
						      :junctive 'NEUTRAL
						      :free-vars nil
						      :parent nil
						      :shallow (natree-assertion natree)
						      :justification 'EQUIV-IMPLICS)))
		     (setf (etree-parent son) new-dup-info
			   (natree-dup-info-neg natree) new-dup-info))))
       (EQUIVIMP (let ((premiss (first support)))
		   (attach-dup-info-to-natree premiss dup-info)
;;;		   (xdisplay-dup-info (natree-dup-info-neg premiss))
		   (setf (natree-dup-info-neg natree)
			 (first (etree-components (update-dup-info (natree-dup-info-neg premiss)))))))
       ((DEFN LAMBDA EQUALITY EQUIV-EQ EQUIVWFFS EXT= SYM= LAMBDA=) ; cebrown 8/22/99 added lambda=, removed REFL=
	(display-dup-info dup-info)
	(let ((premiss (first support)))
	  (attach-dup-info-to-natree premiss 
				     (dup-info-rewrite-exp-ctr T (natree-justification natree) dup-info
							       (initialize-dup-info (natree-assertion premiss) T)))
	  (setf (natree-dup-info-neg natree)
		(dup-info-rewrite-exp-ctr nil (natree-justification natree) (natree-dup-info-neg premiss)
					  (initialize-dup-info (natree-assertion natree) nil)))))
       ((SUB= SUBST=)
	(display-dup-info dup-info)
	(throwfail (msgf "Very sorry, I cannot handle " (natree-justification natree) " yet!")))
       ((RULEP CONJEQUIV EQUIVCONJ)
	(let ((*ignore-statuses* T)
	      (topnode (make-implication :positive nil :junctive 'con))
	      (right (natree-etree natree)) left components)
	  (when (leaf-p right)
		(setf (natree-etree natree)
		      (deepen-to-literals*-rulep right))
		(setq right (natree-etree natree)))
	  (setq components
		(mapcar #'(lambda (natree)
			    (let ((node (natree-etree natree)))
			      (if (leaf-p node)
				  (setf (natree-etree natree)
					(deepen-to-literals*-rulep node)))
			      (natree-etree natree)))
			support))
	  (setq left (cond ((null components) (make-true :positive t :junctive 'CON)) ; cebrown 11/19/00 added :junctive 'CON
			   ((cdr components)
			    (make-econjunction :positive T
					       :junctive 'con
					       :components components))
			   (T (car components))))
	  (setf (etree-components topnode) (list left (negate-etree right)))
	  (let* ((jform (etree-to-jform topnode))
		 (connlist (delete-duplicates (find-rulep-mating jform)
					      :test #'(lambda (x y) (and (eq (car x) (car y))
									 (eq (cdr x) (cdr y)))))))
	    (negate-etree right)
	    (display-vp-diag jform)
	    (let ((etree (natree-etree natree)) heterolist homolist selflist)
	      (dolist (conn connlist)
		      (let ((hans (find-etree-node #'(lambda (x) (eq (car conn) (etree-name x))) etree))
			    (tans (find-etree-node #'(lambda (x) (eq (cdr conn) (etree-name x))) etree)))
			(cond ((and hans tans)
			       (msgf "Attention: a rare case occurred!")
			       (push conn selflist))
			      (hans (push conn heterolist))
			      (tans (push (cons (cdr conn) (car conn)) heterolist))
			      (otherwise (push conn homolist)))))
	      (setf (natree-heterolist natree) heterolist
		    (natree-homolist natree) homolist
		    (natree-selflist natree) selflist))
	    (outfox-rulep natree dup-info))))
       ))))

(defun upward-update-natree (natree dup-info)
;;;  (msgf "I am updating on " (natree-justification natree))
;;;  (msgf "The assertion is " ((natree-assertion natree) . gwff))
  (if (empty-dup-info-p dup-info)
      (msgf "Skipping an empty-dup-info in UPWARD-UPDATE-NATREE!")
  (let ((support (natree-support natree)))
    (ecase (natree-justification natree)
       (UI (upward-update-natree (first support)
				 (expansion-dup-info (natree-assertion (first support))
						     (natree-jterm natree) dup-info T)))
       ;;;everything should go up, right?
       (UGEN (unless (leaf-p dup-info)
		     (dolist (sub-dup-info (etree-components dup-info))
			     (upward-update-natree (first support) sub-dup-info))))
       (CHOOSE (upward-update-natree (first support)
				     (selection-dup-info (natree-assertion (first support))
							 (natree-jterm natree) dup-info T)))
       (RULEC (upward-update-natree (second support) dup-info))
       ;;;everything should go up, right?
       (EGEN (unless (leaf-p dup-info)
		     (dolist (sub-dup-info (etree-components dup-info))
			     (upward-update-natree (first support) sub-dup-info))))
       ((AB |SAME AS|) (upward-update-natree (first support) dup-info))
       ((ABSURD |ASSERT DESCR| |ASSERT REFL=|))
       ((|ASSUME NEGATION| HYP)
	(setf (natree-dup-info-pos natree)
	      (merge-dup-info (natree-dup-info-pos natree) dup-info)))
       (|CASE 1| (upward-update-natree (first support) 
				       (edisjunction-dup-info dup-info
							      (empty-dup-info-1 (cdr (natree-assertion (first support))) T) T)))
       (|CASE 2| (upward-update-natree (first support) 
				       (edisjunction-dup-info (empty-dup-info-1 (cdar (natree-assertion (first support))) T)
							      dup-info T)))
       (CASES (let ((co-dup-info (copy-dup-info dup-info)))
		(upward-update-natree (second support) dup-info)
		(upward-update-natree (third support) co-dup-info)))
       (CONJ (cond ((= 2 (length support))
		    (unless (leaf-p dup-info)
			    (upward-update-natree (first support) (first (etree-components dup-info)))
			    (upward-update-natree (second support) (second (etree-components dup-info)))))
		   ((= 1 (length support))
		    (let ((premiss (first support)))
		      (cond ((wffeq (natree-assertion natree)
				    (cdar (natree-assertion premiss)))
			     (upward-update-natree (first support)
						   (econjunction-dup-info dup-info 
									  (empty-dup-info-1 (cdr (natree-assertion premiss)) T) T)))
			    ((wffeq (natree-assertion natree)
				    (cdr (natree-assertion premiss)))
			     (upward-update-natree (first support)
						   (econjunction-dup-info (empty-dup-info-1 (cdar (natree-assertion premiss)) T)
									  dup-info T)))
			    (otherwise (throwfail (msg ((natree-assertion natree) . gwff)
						       " is not a branch in " 
						       ((natree-assertion premiss) . gwff)))))))))
       ((DEDUCT NEGINTRO)
	(unless (leaf-p dup-info)
		(let* ((premiss (first support))
		       (hypo (first (set-difference (natree-hypo premiss)
						    (natree-hypo natree)))))
		  ;;; make sure hypo is used.
		  (if hypo (let ((dup-info (natree-dup-info-neg hypo))
				 (new-dup-info (first (etree-components dup-info))))
			     (setf (natree-dup-info-neg hypo)
				   (if dup-info (merge-dup-info dup-info new-dup-info) new-dup-info))))
		  (upward-update-natree premiss (second (etree-components dup-info))))))
        ;;; nothing needs to be done since neg-natree-dup-info-with-assertion has been applied.
       ((INDIRECT NEG DISJ-IMP IMP-DISJ))
       (MP (upward-update-natree (second support)
				 (implication-dup-info (empty-dup-info (first support) nil) dup-info T)))
       ((IMPEQUIV EQUIVIMP) (upward-update-natree (first support) dup-info))
       ((DEFN LAMBDA EQUALITY EQUIV-EQ EQUIVWFFS EXT= REFL= SUB= SUBST= LAMBDA=) ; cebrown added LAMBDA= and SUB=
	(unless (leaf-p dup-info)
		(upward-update-natree (first support)
				      (dup-info-rewrite-exp-ctr T (natree-justification natree) dup-info
								(initialize-dup-info (natree-assertion (first support)) T)))))
       ((RULEP CONJEQUIV EQUIVCONJ)
	(let ((rulep-table (make-hash-table :rehash-size 1.5 :size 64)))
	  (declare (special rulep-table))
	  (break-up-dup-info (natree-etree natree) dup-info)
	  (dolist (conn (natree-heterolist natree))
		  (let  ((dup-info (gethash (cdr conn) rulep-table))
			 (new-dup-info (gethash (car conn) rulep-table)))
		    (setf (gethash (cdr conn) rulep-table)
			  (if dup-info (merge-dup-info dup-info new-dup-info) new-dup-info))))
	  (dolist (sup support)
		  (let ((init-rulep-table rulep-table))
		    (declare (special init-rulep-table))
		    (upward-update-natree sup (put-together-dup-info (natree-etree sup) T))))))))))

(defun natree-to-etree ()
  (setq current-eproof (make-eproof :skolem-method skolem-default) master-eproof current-eproof)
  (setf (get (eproof-name current-eproof) 'EPROOF) current-eproof) ; 9/13/01
  (push (eproof-name current-eproof) *eproof-list*) ; 9/13/01
  (initialize-current-natree)
  (setf (eproof-etree current-eproof)
	(make-leaf :shallow (natree-assertion current-natree)
		   :positive nil
		   :parent nil))
  (update-status nil (eproof-etree current-eproof) 1)
  (init-symmetry (eproof-etree current-eproof) current-eproof)
  (attach-dup-info-to-natree current-natree (make-leaf :shallow (natree-assertion current-natree)
						       :positive T
						       :parent nil))
  (msgf "Duplication information has been generated." T
	"Now display the expansion tree.")
  (initialize-mating-search)
  (setq current-topnode (natree-dup-info-neg current-natree))
  (msgf "This is unmerged current-topnode:")
  (display-dup-info current-topnode)
  (msgf "Now merge begins.")
  (merge-all (pre-merge-dup-info current-topnode) nil)
  (let ((*ignore-statuses* T))
    (display-vp-diag (etree-to-jform current-topnode))
    (display-etree-all current-topnode))
  (setf (eproof-etree current-eproof) current-topnode))

(defun initialize-dup-info (assertion pos)
  (make-leaf :positive pos :shallow assertion :parent nil))

(defun deepen-dup-info (node)
  (let ((newnode (deepen-leaf-node-natree node nil)))
    (strip-exp-vars-from-etree newnode)
    newnode))

(defun deepen-dup-info* (node)
  (let ((newnode (deepen-to-literals-natree node nil)))
    (strip-exp-vars-from-etree newnode)
    (let ((*ignore-statuses* T))
      (display-vp-diag (etree-to-jform newnode)))
    newnode))

(defun deepen-leaf-node-natree (node leaf)
  (let ((wff (get-shallow node))
	(free-vars (etree-free-vars node))
	(parent (etree-parent node))
	(positive (positive-p node))
	(rename-all-bd-vars t)
	newnode)
    (declare (special rename-all-bd-vars))
    (setq newnode
	  (cond ((not-p wff)
		 (create-negation-node wff positive free-vars parent node))
		((and-p wff)
		 (create-econjunction-node wff positive free-vars parent node))
		((or-p wff)
		 (create-edisjunction-node wff positive free-vars parent node))
		((implies-p wff)
		 (create-implication-node wff positive free-vars parent node))
		((equiv-p wff)
		 (create-rewrite-node
		  wff positive (rewrite-equiv-natree wff) 'equiv-implics
		  free-vars parent node))
		((wffeq wff 'TRUTH)
		 (create-true-node wff positive free-vars parent node))
		((wffeq wff 'FALSEHOOD)
		 (create-false-node wff positive free-vars parent node))
		((if positive (a-bd-wff-p wff) (e-bd-wff-p wff))
		 (create-expansion-node wff positive free-vars parent node))
		((if positive (e-bd-wff-p wff) (a-bd-wff-p wff))
		 (create-selection-node wff positive free-vars parent node))
		))
    (cond (newnode (setf (etree-leaf newnode) leaf) ;;;this is a crucial step in handling variables and constants.
		   (update-global-lists node newnode) newnode)
	  (T node))))

(defun deepen-to-literals-natree (node leaf)
  (let ((newnode (if (leaf-p node) (deepen-leaf-node-natree node leaf) node)))
    (dolist (subnode (etree-components newnode) newnode)
      (deepen-to-literals-natree subnode leaf))))

(defun etree-conc-attach-natree (natree)
  (if (not (natree-etree natree))
      (let ((support (natree-support natree)))
	(setf (natree-etree natree)
	      (make-leaf :shallow (natree-assertion natree)
			 :positive T
			 :parent nil))
  	(dolist (sup support)
		(push natree (natree-conclusion sup))
		(etree-conc-attach-natree sup)))))

(defun initialize-current-natree ()
  (etree-conc-attach-natree current-natree))

;(defun name-skolem-cap (var new-type)
;  (let* ((nameroot (nameroot var))
;	 (face (or (get var 'face) (get (intern nameroot) 'face))))
;    (funcall ren-var-fn
;	     (intern (string-capitalize (if face (string (car face)) nameroot)))
;	     new-type)))
;already in wff-skolem.lisp


;;; Basically this is a pretty risky function. Make sure that
;;; (get-shallow dup-info) and (if pos assertion (cons 'NOT assertion)
;;; can be transformed back and forth by the following rules:
;;; NEG, DISJ-IMP, IMP-DISJ

(defun neg-dup-info-with-assertion (dup-info assertion pos)
  (let* ((new-dup-info dup-info))
    (cond ((empty-dup-info-p dup-info))
	  ((not-p assertion)
	   (cond ((negation-p dup-info)
		  (let ((sub-new-dup-info (neg-dup-info-with-assertion (first (etree-components dup-info))
								       (cdr assertion) pos)))
		    (setf (etree-components new-dup-info) (list sub-new-dup-info)
			  (etree-parent sub-new-dup-info) new-dup-info)))
		 (otherwise (setq new-dup-info (make-negation :components nil
							      :positive pos
							      :junctive 'NEUTRAL
							      :free-vars nil
							      :parent nil))
			    (let ((sub-new-dup-info (neg-dup-info-with-assertion dup-info (cdr assertion) (not pos))))
			      (setf (etree-components new-dup-info) (list sub-new-dup-info)
				    (etree-parent sub-new-dup-info) new-dup-info)))))
	  ((rewrite-p dup-info)
	   (setq new-dup-info (neg-dup-info-with-assertion (first (etree-components dup-info))
							   assertion pos)))
	  ((negation-p dup-info)
	   (setq new-dup-info (neg-dup-info-with-assertion (first (etree-components dup-info))
							   assertion (not pos))))
	  ((true-p dup-info)
	   (cond ((eq (quote falsehood) assertion)
		  (setq new-dup-info (make-false :components nil
						 :positive pos
						 :junctive (if pos 'DIS 'CON)
						 :free-vars nil
						 :parent nil)))
		 ((eq (quote truth) assertion))
		 (otherwise (describe dup-info) (msgf assertion)
			    (throwfail t "unexpected case in TRUE-P case of neg-dup-info-with-assertion!!!"))))
	  ((false-p dup-info)
	   (cond ((eq (quote truth) assertion)
		  (setq new-dup-info (make-true :components nil
						:positive pos
						:junctive (if pos 'CON 'DIS)
						:free-vars nil
						:parent nil)))
		 ((eq (quote falsehood) assertion))
		 (otherwise (describe dup-info) (msgf assertion)
			    (throwfail "unexpected case in FALSE-P case of neg-dup-info-with-assertion!!!"))))
 
	  ((expansion-p dup-info)
	   (unless (wffeq (expansion-shallow dup-info) assertion) 
		   (setf (etree-positive new-dup-info) pos
			 (expansion-shallow new-dup-info) assertion)
		   (let (newcomps)
		     (do ((terms (expansion-terms dup-info) (cdr terms))
			  (comps (etree-components dup-info) (cdr comps)))
			 ((null terms) (setq newcomps (nreverse newcomps)))
			 (push (neg-dup-info-with-assertion (car comps)
							    (substitute-l-term-var (car terms) 
										   (caar assertion)
										   (cdr assertion)) 
							    pos)
			       newcomps))
		     (setf (etree-components new-dup-info) newcomps))))
	  ((selection-p dup-info)
	   (unless (wffeq (selection-shallow dup-info) assertion)
		   (setf (etree-positive new-dup-info) pos
			 (selection-shallow new-dup-info) assertion)
		   (let (newcomps)
		     (dolist (comp (etree-components dup-info) (nreverse newcomps))
			     (push (neg-dup-info-with-assertion comp (cdr assertion) pos) newcomps))
		     (setf (etree-components new-dup-info) newcomps))))
	 ((and-p assertion)
	   (let ((comp (etree-components dup-info)))
	   (cond ((econjunction-p dup-info)
		  (setf (etree-components new-dup-info)
			(list (neg-dup-info-with-assertion (first comp) (cdar assertion) pos)
			      (neg-dup-info-with-assertion (second comp) (cdr assertion) pos))))
		 ((edisjunction-p dup-info)
		  (let ((lson (neg-dup-info-with-assertion (first comp) (cdar assertion) pos))
			(rson (neg-dup-info-with-assertion (second comp) (cdr assertion) pos)))
		    (setq new-dup-info
			  (make-econjunction :components (list lson rson)
					     :positive pos
					     :junctive (if pos 'CON 'DIS)
					     :free-vars nil
					     :parent nil))
		    (setf (etree-parent lson) new-dup-info
			  (etree-parent rson) new-dup-info)))
		 ((implication-p dup-info)
		  (let ((lson (neg-dup-info-with-assertion (first comp) (cdar assertion) (not pos)))
			(rson (neg-dup-info-with-assertion (second comp) (cdr assertion) pos)))
		    (setq new-dup-info
			  (make-econjunction :components (list lson rson)
					     :positive pos
					     :junctive (if pos 'CON 'DIS)
					     :free-vars nil
					     :parent nil))
		    (setf (etree-parent lson) new-dup-info
			  (etree-parent rson) new-dup-info)))
		 (otherwise (describe dup-info) (msgf assertion)
			    (throwfail t "unexpected case in AND-P case of neg-dup-info-with-assertion!!!")))))
	 ((or-p assertion)
	  (let ((comp (etree-components dup-info)))
	    (cond ((econjunction-p dup-info)
		   (let ((lson (neg-dup-info-with-assertion (first comp) (cdar assertion) pos))
			 (rson (neg-dup-info-with-assertion (second comp) (cdr assertion) pos)))
		     (setq new-dup-info
			  (make-edisjunction :components (list lson rson)
					     :positive pos
					     :junctive (if pos 'DIS 'CON)
					     :free-vars nil
					     :parent nil))
		     (setf (etree-parent lson) new-dup-info
			   (etree-parent rson) new-dup-info)))
		  ((edisjunction-p dup-info)
		   (setf (etree-components new-dup-info)
			 (list (neg-dup-info-with-assertion (first comp) (cdar assertion) pos)
			       (neg-dup-info-with-assertion (second comp) (cdr assertion) pos))))
		  ((implication-p dup-info)
		   (let ((lson (neg-dup-info-with-assertion (first comp) (cdar assertion) (not pos)))
			 (rson (neg-dup-info-with-assertion (second comp) (cdr assertion) pos)))
		     (setq new-dup-info
			   (make-edisjunction :components (list lson rson)
					      :positive pos
					      :junctive (if pos 'DIS 'CON)
					      :free-vars nil
					      :parent nil))
		     (setf (etree-parent lson) new-dup-info
			   (etree-parent rson) new-dup-info)))
		  (otherwise (describe dup-info) (msgf assertion)
			     (throwfail "unexpected case in OR-P case of neg-dup-info-with-assertion!!!")))))
	 ((implies-p assertion)
	  (let ((comp (etree-components dup-info)))
	    (cond ((econjunction-p dup-info)
		   (let ((lson (neg-dup-info-with-assertion (first comp) (cdar assertion) (not pos)))
			 (rson (neg-dup-info-with-assertion (second comp) (cdr assertion) pos)))
		     (setq new-dup-info
			  (make-implication :components (list lson rson)
					    :positive pos
					    :junctive (if pos 'DIS 'CON)
					    :free-vars nil
					    :parent nil))
		     (setf (etree-parent lson) new-dup-info
			   (etree-parent rson) new-dup-info)))
		  ((edisjunction-p dup-info)
		   (let ((lson (neg-dup-info-with-assertion (first comp) (cdar assertion) (not pos)))
			 (rson (neg-dup-info-with-assertion (second comp) (cdr assertion) pos)))
		     (setq new-dup-info
			   (make-implication :components (list lson rson)
					     :positive pos
					     :junctive (if pos 'DIS 'CON)
					     :free-vars nil
					     :parent nil))
		     (setf (etree-parent lson) new-dup-info
			   (etree-parent rson) new-dup-info)))
		  ((implication-p dup-info)
		   (setf (etree-components new-dup-info)
			 (list (neg-dup-info-with-assertion (first comp) (cdar assertion) pos)
			       (neg-dup-info-with-assertion (second comp) (cdr assertion) pos))))
		  (otherwise (describe dup-info) (msgf assertion)
			     (throwfail "unexpected case in IMPLIES-P case of neg-dup-info-with-assertion!!!")))))
	 ((leaf-p dup-info))
;;;the following are new. MB Tue Apr 29 16:08:24 1997
	 ((empty-dup-info-p (car (etree-components dup-info)))
	  (setf (etree-components (etree-parent dup-info)) 
		(mapcar #'(lambda (x) (if (eq x dup-info) (car (etree-components dup-info)) x))
			(etree-components (etree-parent dup-info))))
	  (setf (etree-parent (car (etree-components dup-info))) (etree-parent dup-info))
	  (neg-dup-info-with-assertion (car (etree-components dup-info)) assertion pos))
	 ((empty-dup-info-p (cadr (etree-components dup-info)))
	  (setf (etree-components (etree-parent dup-info)) 
		(mapcar #'(lambda (x) (if (eq x dup-info) (cadr (etree-components dup-info)) x))
			(etree-components (etree-parent dup-info))))
	  (setf (etree-parent (cadr (etree-components dup-info))) (etree-parent dup-info))
	  (neg-dup-info-with-assertion (cadr (etree-components dup-info)) assertion pos))
	 (otherwise (describe dup-info) (msgf assertion)
		    (throwfail "unexpected case in neg-dup-info-with-assertion!!!")))
    new-dup-info))
