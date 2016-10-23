;;; -*- Mode:LISP; Package:ML -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :ml)
(part-of ml-etr-tactics)

(context etr-nat)

(deffile ml-nat-etr2
  (extension lisp)
  (part-of ml-etr-tactics)
  (mhelp "Functions for translating from natural deduction
proofs to expansion proofs."))


(defun xlate-same (line just-lines just-terms assertion)
  (declare (ignore just-terms assertion)
	   (special mate-list line-node-list))
  (let ((node (justified-by-node line)))
  (unless (justified-by-node (car just-lines))
	  (nat-xlate (car just-lines)))
  (if (complementary-p node (justified-by-node (car just-lines)))
      (push (cons (justified-by-node line) 
		  (justified-by-node (car just-lines)))
	    mate-list)
    )))

(defun complementary-p (node1 node2)
  (let ((wff1 (if (etree-positive node1) 
		   (get-shallow node1)
		 (cons 'not (get-shallow node1))))
	 (wff2 (if (etree-positive node2)
		    (get-shallow node2)
		  (cons 'not (get-shallow node2))))
	 (root1 nil)
	 (root2 nil)
	 (num1 0)
	 (num2 0))
    (multiple-value-setq (root1 num1) (get-root-and-negs wff1))
    (multiple-value-setq (root2 num2) (get-root-and-negs wff2))    
    (and (wffeq root1 root2)
	 (or (and (oddp num1) (evenp num2))
	     (and (oddp num2) (evenp num1))))))

(defun get-root-and-negs (wff)
  (do ((root wff (gdr root))
       (num 0 (1+ num)))
      ((not (not-p root)) (values root num))))
	


#+comment(defun xlate-deduct (line just-lines just-terms assertion)
  (declare (ignore just-terms)
	   (special mate-list line-node-list))
  (let* ((node (justified-by-node line))
	 (pos (if node (positive-p node) t))
	 (imp-node (if (implication-p node) 
		       node
		     (make-implication :positive pos
				       :parent (etree-parent node)
				       :junctive (if pos 'dis 'con))))
 	 (left-node (if (implication-p node)
			(car (etree-components node))
		      (make-leaf :positive (not pos) :parent imp-node
				 :shallow (gdr (gar assertion)))))
	 (right-node (if (implication-p node)
			 (cadr (etree-components node))
		       (make-leaf :positive pos :parent imp-node
				  :shallow (gdr assertion))))
	 (left-line (car (set-difference (line-hypotheses (car just-lines))
					 (line-hypotheses line))))
	 (right-line (car just-lines))) 
    (if left-line (update-line-node-list left-line left-node))
    (update-line-node-list right-line right-node)
    (setf (etree-components imp-node) (list left-node right-node))
    (update-line-node-list line imp-node)
    (when (etree-parent node)
	  (setf (etree-components (etree-parent node))
		(nsubstitute imp-node node (etree-components (etree-parent node)))))
    (nat-xlate right-line)
    ))

(defun xlate-deduct (line just-lines just-terms assertion)
  (declare (ignore just-terms assertion)
	   (special mate-list line-node-list))
  (let* ((node (justified-by-node line))
	 (imp-node (if (implication-p node) 
		       node
		     (deepen-leaf-node-rulep node)))
 	 (left-node (car (etree-components imp-node)))
	 (right-node (cadr (etree-components imp-node)))
	 (left-line (car (set-difference (line-hypotheses (car just-lines))
					 (line-hypotheses line))))
	 (right-line (car just-lines))) 
    (if left-line (update-line-node-list left-line left-node))
    (update-line-node-list right-line right-node)
    (update-line-node-list line imp-node)
    (when (etree-parent node)
	  (setf (etree-components (etree-parent node))
		(nsubstitute imp-node node (etree-components (etree-parent node)))))
    (nat-xlate right-line)
    ))



(defun xlate-cases (line just-lines just-terms assertion)
  (declare (ignore just-terms assertion)
	   (special mate-list line-node-list))
  (let* ((node (justified-by-node line))
	 (disj (car just-lines))
	 (conc1 (cadr just-lines))
	 (conc2 (caddr just-lines)))
    (unless (justified-by-node disj)
      (nat-xlate disj))
    (update-line-node-list conc1 node)
    (update-line-node-list conc2 node)
    (nat-xlate conc1)
    (nat-xlate conc2)
   ))

(defun xlate-case (line just-lines just-terms assertion)
  (declare (ignore just-terms assertion)
	   (special mate-list line-node-list))
  (let* ((disj (car just-lines))
	 (disj-node (justified-by-node disj))
	 (left-node nil)
	 (right-node nil))
    (unless disj-node
      (nat-xlate disj)
      (setq disj-node (justified-by-node disj)))
    (unless (eq 'dis (etree-junctive disj-node))
      (setq disj-node (deepen-leaf-node-rulep disj-node))
      (update-line-node-list disj disj-node)
      )
    (setq left-node (car (etree-components disj-node)))
    (setq right-node (cadr (etree-components disj-node)))      
    (if (string-equal (line-just-rule line) "Case 1")
	(update-line-node-list line left-node)
        (update-line-node-list line right-node))))



(defun xlate-econj (line just-lines just-terms assertion)
  (declare (ignore just-terms)
	   (special mate-list line-node-list))
  (let* ((conj (car just-lines))
	 (conj-node (justified-by-node conj))
	 (left-node nil)
	 (right-node nil))
    (unless conj-node
      (nat-xlate conj)
      (setq conj-node (justified-by-node conj)))
    (unless (eq 'con (etree-junctive conj-node))
      (setq conj-node (deepen-leaf-node-rulep conj-node))
      (update-line-node-list conj conj-node)
      )
    (setq left-node (car (etree-components conj-node)))
    (setq right-node (cadr (etree-components conj-node)))      
    (if (wffeq assertion (auto::strip-sk-terms
			  (auto::strip-exp-vars 
			   (get-shallow left-node))))
	(update-line-node-list line left-node)
      (update-line-node-list line right-node))))



(defun xlate-iconj (line just-lines just-terms assertion)
  (declare (ignore just-terms assertion)
	   (special mate-list line-node-list))
  (let* ((conj line)
	 (conj-node (justified-by-node conj))
	 (left-node nil)
	 (right-node nil))
    (unless (eq 'dis (etree-junctive conj-node))
      (setq conj-node (deepen-leaf-node-rulep conj-node))
      (update-line-node-list conj conj-node)
      )
    (setq left-node (car (etree-components conj-node)))
    (setq right-node (cadr (etree-components conj-node)))      
    (update-line-node-list (car just-lines) left-node)
    (update-line-node-list (cadr just-lines) right-node)
    (nat-xlate (car just-lines))
    (nat-xlate (cadr just-lines))))

(defun xlate-egen (line just-lines just-terms assertion)
  (declare (ignore assertion) (special mate-list line-node-list))
  (let* ((egen-node (justified-by-node line)))
    (if  (not (expansion-p egen-node))
	(progn
	  (setq egen-node (deepen-leaf-node egen-node))
	  #+comment(substitute-in-etree-main (car just-terms)
				    (car (expansion-terms egen-node))
				    egen-node)
	  (substitute-in-etree (car just-terms)
			       (car (expansion-terms egen-node))
			       egen-node)
	  (update-line-node-list line egen-node)
	  )
      (progn 
	(duplicate-var egen-node)
	#+comment(substitute-in-etree-main (car just-terms)
				    (car (last (expansion-terms egen-node)))
				    egen-node)
	(substitute-in-etree (car just-terms)
			     (car (last (expansion-terms egen-node)))
			     egen-node)))
    (update-line-node-list (car just-lines) 
			   (car (last (etree-components egen-node))))
    (nat-xlate (car just-lines))))

(defun xlate-ugen (line just-lines just-terms assertion)
  (declare (ignore assertion)(special mate-list line-node-list))
  (let* ((ugen-node (justified-by-node line)))
    (unless (selection-p ugen-node)
      (setq ugen-node (deepen-leaf-node ugen-node))
      (update-line-node-list line ugen-node)
      #+comment(substitute-in-etree-main
	(car just-terms) (car (selection-terms ugen-node))
	ugen-node)
      (setf (skolem-term-term (car (selection-terms ugen-node)))
	    (car just-terms) 
	    (skolem-term-parameter (car (selection-terms ugen-node)))
	    (car just-terms))
      )
    (update-line-node-list (car just-lines) (car (etree-components ugen-node)))
    (nat-xlate (car just-lines))))


(defun xlate-ui (line just-lines just-terms assertion)
  (declare (special mate-list line-node-list) (ignore assertion))
  (let ((univ-node (justified-by-node (car just-lines))))
    (when (not univ-node)
      (nat-xlate (car just-lines))
      (setq univ-node (justified-by-node (car just-lines))))
    (if (not (expansion-p univ-node))
	(progn
	  (setq univ-node (deepen-leaf-node univ-node))
	  #+comment(substitute-in-etree-main (car just-terms) 
				    (car (expansion-terms univ-node))
				    univ-node)
	  (substitute-in-etree (car just-terms) 
			       (car (expansion-terms univ-node))
			       univ-node)
	  (update-line-node-list (car just-lines) univ-node)
      )
      (progn
	(duplicate-var univ-node)
	(substitute-in-etree (car just-terms)
			     (car (last (expansion-terms univ-node)))
			     univ-node)))
    (update-line-node-list line (car (last (etree-components univ-node))))
))


(defun xlate-rulec (line just-lines just-terms assertion)
  (declare (ignore just-terms assertion) (special mate-list line-node-list))
  (let* ((node (justified-by-node line))
	 (sel-line (car just-lines))
	 (sel-node (justified-by-node sel-line))
	 (conc-line (cadr just-lines))
	 (choice-line (car (set-difference (line-hypotheses conc-line)
					   (line-hypotheses line))))
	 (choice-var (car (line-just-terms choice-line)))
	 (choice-node nil))
    (when (not sel-node)
      (nat-xlate sel-line)
      (setq sel-node (justified-by-node sel-line)))
    (if (not (selection-p sel-node))
	(progn
	  (setq sel-node (deepen-leaf-node sel-node))
	  (update-line-node-list sel-line sel-node)
	  #+comment(substitute-in-etree-main choice-var
			       (car (selection-terms sel-node))
			       (car (etree-components sel-node)))
	  (setf (skolem-term-term (car (selection-terms sel-node)))
	    choice-var
	    (skolem-term-parameter (car (selection-terms sel-node)))
	    choice-var)
	  #+comment(setf (selection-terms sel-node) (list choice-var))
	  (setq choice-node (car (etree-components sel-node))))
      ;; this is probably bogus here.
       (progn 
	 (setq choice-node (make-leaf :positive (positive-p sel-node)
				      :parent sel-node
				      :shallow 
				      (substitute-term-var 
					choice-var 
					(bindvar (selection-shallow sel-node))
					(gdr (selection-shallow sel-node)))))
	 (setf (etree-junctive sel-node) 'con)
	 (push choice-var (selection-terms sel-node))
	 (push choice-node (etree-components sel-node))))
    (update-line-node-list choice-line choice-node)
    (update-line-node-list conc-line node)
    (nat-xlate conc-line)))

#+comment(defun xlate-mp (line just-lines just-terms assertion)
  (declare (ignore just-terms assertion)(special mate-list line-node-list))
  (let* ((right-node nil)
	 (imp-line (cadr just-lines))
	 (imp-node (justified-by-node imp-line))
	 (ant-line (car just-lines))
	 (ant-node (justified-by-node ant-line))
	 (left-node nil))
    (when (not ant-node)
	  (nat-xlate ant-line)
	  (setq ant-node (justified-by-node ant-line)))
    (when (not imp-node)
	  (nat-xlate imp-line)
	  (setq imp-node (justified-by-node imp-line)))
    (when (not (implication-p imp-node))
	  (update-line-node-list
	    imp-line
	    (make-implication :positive (positive-p imp-node)
			      :parent (etree-parent imp-node)
			      :junctive (if (positive-p imp-node) 'dis 'con)
			      :components 
			      (list (make-leaf :shallow
					       (gdr (gar (line-assertion
							   imp-line)))
					       :positive
					       (not (positive-p imp-node)))
				    (make-leaf :shallow
					       (gdr (line-assertion
						      imp-line))
					       :positive
					       (positive-p imp-node))
				    )))
	  (setq imp-node (justified-by-node imp-line)))
    (setq left-node (car (etree-components imp-node)))
    (setq right-node (cadr (etree-components imp-node)))
    (setf (etree-parent left-node) imp-node)
    (setf (etree-parent right-node) imp-node)
    (push (cons left-node ant-node) mate-list)
    (update-line-node-list line right-node)))

(defun xlate-mp (line just-lines just-terms assertion)
  (declare (ignore just-terms assertion)(special mate-list line-node-list))
  (let* ((right-node nil)
	 (imp-line (cadr just-lines))
	 (imp-node (justified-by-node imp-line))
	 (ant-line (car just-lines))
	 (ant-node (justified-by-node ant-line))
	 (left-node nil))
    (when (not ant-node)
	  (nat-xlate ant-line)
	  (setq ant-node (justified-by-node ant-line)))
    (when (not imp-node)
	  (nat-xlate imp-line)
	  (setq imp-node (justified-by-node imp-line)))
    (when (not (implication-p imp-node))
      (setq imp-node (deepen-leaf-node-rulep imp-node))
      (update-line-node-list imp-line imp-node)
      )
    (setq left-node (car (etree-components imp-node)))
    (setq right-node (cadr (etree-components imp-node)))
    (push (cons left-node ant-node) mate-list)
    (update-line-node-list line right-node)
    ))
	 


(defun xlate-rewrite (line just-lines assertion justification)
  (declare (special mate-list line-node-list))
  (let* ((node (justified-by-node line))
	 (top-line (car just-lines))
	 (top-node (justified-by-node top-line)))
    (if node
	(progn
	  (update-line-node-list
	    line
	    (make-rewrite :shallow assertion
			  :positive (positive-p node)
			  :junctive 'neutral
			  :justification justification
			  :parent (etree-parent node)))
	  (setq node (justified-by-node line))
	  (update-line-node-list 
	    top-line 
	    (make-leaf :shallow (line-assertion top-line)
		       :positive (positive-p node)
		       :parent node))
	  (nat-xlate top-line)
	  (setf (etree-components node) (list (justified-by-node top-line))))
        (progn
	  (unless top-node
	     (nat-xlate top-line)
	     (setq top-node (justified-by-node top-line)))
	  (setq node
		(make-leaf :shallow assertion :positive (positive-p top-node)))
	  (update-line-node-list
	    top-line
	    (make-rewrite :shallow (get-shallow top-node)
			  :positive (positive-p top-node)
			  :junctive 'neutral
			  :justification justification
			  :parent (etree-parent top-node)
			  :components (list node)))
	  (setq top-node (justified-by-node top-line))
	  (setf (etree-parent node) top-node)
	  (update-line-node-list line node)))))

(defun xlate-pullneg (line just-lines assertion)
  (declare (ignore assertion)(special mate-list line-node-list))
  (let* ((node (justified-by-node line))
	 (top-line (car just-lines))
	 (top-node (justified-by-node top-line)))
    (if node
	(progn
	  (when (leaf-p node)
	    (setq node (deepen-leaf-node-rulep node))
	    (update-line-node-list line node))
	  (setq top-node (if (negation-p node) 
			     (car (etree-components node))
			   node))
	  (update-line-node-list top-line top-node)
	  (nat-xlate top-line)
	  )
        (throwfail "Can't translate line " (linealias line) "(XLATE-PULLNEG).")))
      )

(defun xlate-pushneg (line just-lines assertion)
  (declare (ignore assertion)(special mate-list line-node-list))
  (let* ((top-line (car just-lines))
	 (top-node (justified-by-node top-line)))
    (when (not top-node)
	  (nat-xlate top-line)
	  (setq top-node (justified-by-node top-line)))
    (when (leaf-p top-node)
	  (setq top-node (deepen-leaf-node-rulep top-node))
	  (update-line-node-list top-line top-node))
    (if (negation-p top-node)
	(update-line-node-list line (car (etree-components top-node)))
	(update-line-node-list line top-node))        
      ))

(defun xlate-subst (line just-lines just-terms assertion)
  (declare (ignore assertion)(special mate-list line-node-list))
  (let* ((node (justified-by-node line))
	 (top-line (car just-lines))
	 (top-node (justified-by-node top-line)))
    (if node
	(throwfail "Can't translate line " (linealias line) 
		   "(XLATE-SUBST).")
        (progn
	  (unless top-node
	     (nat-xlate top-line)
	     (setq top-node (justified-by-node top-line)))
	  (setq node (apply-sub-to-etree top-node 
					 (car just-terms) (cadr just-terms)))
	  (update-line-node-list line node)))))



(defun xlate-indirect2 (line just-lines)
  (declare (special mate-list line-node-list))
  (let* ((node (justified-by-node line))
	 (hyps (line-hypotheses line))
	 (pos-line (car just-lines))
	 (neg-line (cadr just-lines))
	 (assumed-negation (car (set-difference 
				  (union (line-hypotheses pos-line)
					 (line-hypotheses neg-line))
				  hyps))))
    (when (and node assumed-negation)
	  (update-line-node-list assumed-negation node))
    (unless (justified-by-node pos-line) (nat-xlate pos-line))
    (unless (justified-by-node neg-line) (nat-xlate neg-line))
    (push (cons (justified-by-node pos-line) (justified-by-node neg-line))
	  mate-list)))

(defun xlate-ineg (line just-lines)
  (declare (special mate-list line-node-list))
  (let* ((node (justified-by-node line))
	 (hyps (line-hypotheses line))
	 (false-line (car just-lines))
	 (assumption (car (set-difference 
			    (line-hypotheses false-line)
			    hyps))))
    (when (and node assumption)
      (unless (auto::negation-p node)
	(setq node (deepen-leaf-node-rulep node))
	(update-line-node-list line node))
      (update-line-node-list assumption (car (etree-components node))))
    (nat-xlate false-line)
))

(defun xlate-indirect (line just-lines)
  (declare (special mate-list line-node-list))
  (let* ((node (justified-by-node line))
	 (neg-node (auto::make-negation :positive (not (positive-p node))
					:components (list node)
					:junctive 'auto::neutral))
	 (hyps (line-hypotheses line))
	 (false-line (car just-lines))
	 (assumed-negation (car (set-difference 
				   (line-hypotheses false-line)
				   hyps))))
    (when (and node assumed-negation)
	  (update-line-node-list assumed-negation neg-node))
    (nat-xlate false-line)
))



(defun xlate-absurd (line just-lines)
  (declare (special mate-list line-node-list) (ignore line))
  (let ((false-line (car just-lines)))
    (nat-xlate false-line)
    (unless (or (not (justified-by-node false-line))
		(auto::false-p (justified-by-node false-line)))
      (update-line-node-list false-line (auto::deepen-to-literals*-rulep 
					  (justified-by-node false-line))))
))

(defun xlate-eneg (line just-lines)
  (declare (special mate-list line-node-list) (ignore line))
  (let* ((neg-line (car just-lines))
	 (pos-line (cadr just-lines))
	 (neg-node (justified-by-node neg-line))
	 (pos-node (justified-by-node pos-line)))
    (when (not neg-node)
	  (nat-xlate neg-line)
	  (setq neg-node (justified-by-node neg-line)))
    (when (not pos-node)
      (nat-xlate pos-line)
      (setq pos-node (justified-by-node pos-line)))
    (unless (auto::negation-p neg-node)
      (setq neg-node (deepen-leaf-node-rulep neg-node))
      (update-line-node-list neg-line neg-node))
;    (update-line-node-list pos-line (car (etree-components neg-node)))
    (update-line-node-list pos-line pos-node)
    (push (cons (justified-by-node pos-line) (justified-by-node neg-line))
	  mate-list)))