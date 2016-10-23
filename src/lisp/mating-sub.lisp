;;; -*- Mode:LISP; Package:auto -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of ms88)

(deffile mating-sub
  (part-of ms88)
  (extension lisp)
  (mhelp "Functions to call mating search procedure MS88 on subtrees."))

(context ms88)

(defmateop ms88
  (mate-alias ms88-real); was ms88-controller)
  (mate-result-> ignore)
  (mhelp "Call mating search procedure on the current eproof.  This
procedure uses a naive level-saturation method, exhaustively searching
a single jform before applying any duplications. Quantifier duplications
are applied uniformly to outermost quantifiers. Will try primitive
substitution for outermost variable only.  Works on only a single
jform at a time."))

(defun ms88-real ()
  (let ((default-ms 'ms88))
    (matingsearch-controller)))

(defmateop ms88-sub
  (mate-alias ms88-sub)
  (mate-result-> ignore))


(defwffop ms88-sub
  (argnames etree)
  (argtypes symbol)
  (resulttype ignore)
  (defaultfns (lambda (etree) (if (eq etree '$)
				  (list (etree-name current-topnode))
				  (list etree))))
  (mhelp "Call MS88 on a partial expansion tree (subtree)."))

(defun ms88-sub (&optional (etree-name nil)
			 (allow-duplications t))
  (let ((etree (if etree-name
		   (if (etree-p etree-name) etree-name
		       (find-etree-node-name
			 etree-name (eproof-etree current-eproof)))
		   current-topnode)))
    (if etree
	(let ((mating-list (mating-list))
	      (max-incomp-clists-wrt-etree (max-incomp-clists-wrt-etree))
	      (incomp-clists-wrt-etree (incomp-clists-wrt-etree))
	      (incomp-clists (incomp-clists))
	      (duplicated nil)
	      (success nil)
	      (bktrack-limit (eproof-bktrack-limit current-eproof))
	      active-mating)
	  (declare (special active-mating duplicated))
	  (unwind-protect
	      (progn (setf (mating-list) (list (init-mating)))
		     (setf (max-incomp-clists-wrt-etree) nil)
		     (setf (incomp-clists-wrt-etree) nil)
		     (setf (incomp-clists) nil)
		     (setf (eproof-bktrack-limit current-eproof)
			   initial-bktrack-limit)
		     (setq active-mating
			   (ms-director etree allow-duplications))
		     (setq success t)
		     active-mating)
	    (setf (mating-list) mating-list)
	    (setf (max-incomp-clists-wrt-etree) max-incomp-clists-wrt-etree)
	    (setf (incomp-clists-wrt-etree) incomp-clists-wrt-etree)
	    (setf (incomp-clists) incomp-clists)
	    (setf (eproof-bktrack-limit current-eproof)
		  bktrack-limit)
	    (when success (push active-mating (mating-list)))))
	(complain etree-name " not found in current-eproof."))))

(defvar num-of-splitting 0)

(defun ms-top (etree)
  (if (etree-components* etree)
      (if (cdr (etree-components* etree))
	  (case (etree-junctive* etree)
	    (dis
	      (msg t "Splitting the problem.")
              (incf num-of-splitting)
	      (compose-matings
		(progn (msg t "Considering etree: "
			    (etree-name* (car (etree-components* etree))))
		       (ms-top (car (etree-components* etree))))
		(progn (decf num-of-splitting)
                       (msg t "Considering etree: "
			    (etree-name* (cadr (etree-components* etree))))
		       (ms-top (cadr (etree-components* etree))))))
	    (con (ms88-sub etree))
	    (t (throwfail "No action specified for junctive property "
			  (etree-junctive etree) ". Node "
			  (etree-name etree))))
	  (if (expansion-p etree)
	      (if first-order-mode-ms (ms88-sub etree t)
		(let ((mating (ms88-sub etree nil))
		      (var (car (etree-components* etree))))
		  (if mating mating
		    (progn
		      (msg t "Considering primitive substitutions "
			   "for variable: "(bdvar (get-shallow etree)))
		      (apply-prim-subs-outer etree)
		      (do ((comps (etree-components* etree) (cdr comps))
			   (terms (sel-exp-terms etree) (cdr terms))
			   (son))
			  ((null terms))
			(unless (eq (car comps) var)
			  (msg t 3 "Primitive Substitution: "
			       ((car terms) . gwff))
			  (vpwin-update-seven (car terms))
			  (let ((renumber-leaves nil))
			    (setq son (deepen-to-literals* (car comps))))
			  (setq mating (ms88-sub son nil))
			  (if mating (return-from ms-top mating)
					;(setf (etree-status son) 0)
			    #+comment(push (cons (etree-name* son) 0)
					   (eproof-statuses
					    current-eproof))
			    (update-status nil son 0))))
		      (let ((dup-strategy
			     (if ;interrupt-enable 
				 (eq query-user T) ;Changed 22MAY90 DAN; and again 7/23/93 MB
				 (if (query "Duplication required. Continue?"
					    '$)
				     duplication-strategy 'abort)
			       (progn (msg T "Duplicating")
				      duplication-strategy))))
			(case dup-strategy
			  (dup-outer (duplicate-all-outer-vars etree))
			  (dup-all (duplicate-all-vars etree))
			  (t (throwfail "Search aborted."))))
		      (setq etree (deepen-after-duplication etree))
		      (ms88-sub etree t)))))
	    (ms-top (car (etree-components* etree)))))
    (ms88-sub etree t)))

(defun reduce-subst-stack (subst-stack)
  (let ((h-vars nil)
	(reduced-substs nil))
    (dolist (subst (reverse subst-stack) reduced-substs)
      (when (not (memq (car subst) h-vars))
	(push (cons (car subst) 
		    (make-subst :term (lambda-reduce-subst (cdr subst) 
							   subst-stack)
				:h-vars nil
				:type 's
				:new-h-vars nil))
	      reduced-substs))
      (setq subst (cdr subst))
      (when (subst-p subst)
	(if (subst-new-h-vars subst)
	    (dolist (sub (subst-new-h-vars subst))
	      (push (cdr sub) h-vars))
	    (dolist (var (subst-h-vars subst))
	      (push var h-vars)))))))


(defun compose-matings (mating1 mating2)
  (if mating1
      (if mating2
	  (let ((active-mating (init-mating)))
	    (setf (mating-clist active-mating)
		  (nconc (mating-clist mating1) (mating-clist mating2)))
	    (when (or (mating-utree mating1) (mating-utree mating2))
	      (setf (mating-utree active-mating)
		    (if first-order-mode-ms
			(nconc (mating-utree mating1) (mating-utree mating2))
			(let ((node (make-node
				      :dpairs nil :print-name "0"
				      :subst-stack
				      (nconc (reduce-subst-stack 
					       (node-subst-stack
						 (car (mating-success-nodes
							mating1))))
					     (reduce-subst-stack
					       (node-subst-stack
						 (car (mating-success-nodes
							mating2))))))))
			  (setf (mating-success-nodes active-mating)
				(list node))
			  node))))
	    (setf (stats-num-added (mating-stats active-mating))
		  (+ (stats-num-added (mating-stats mating1))
		     (stats-num-added (mating-stats mating2))))
	    (setf (stats-num-removed (mating-stats active-mating))
		  (+ (stats-num-removed (mating-stats mating1))
		     (stats-num-removed (mating-stats mating2))))
	    (setf (stats-num-conns-considered (mating-stats active-mating))
		  (+ (stats-num-conns-considered (mating-stats mating1))
		     (stats-num-conns-considered (mating-stats mating2))))
	    (setf (stats-num-dupes (mating-stats active-mating))
		  (+ (stats-num-dupes (mating-stats mating1))
		     (stats-num-dupes (mating-stats mating2))))
	    (setf (stats-num-vars-duped (mating-stats active-mating))
		  (+ (stats-num-vars-duped (mating-stats mating1))
		     (stats-num-vars-duped (mating-stats mating2))))
	    (setf (stats-num-primsubs (mating-stats active-mating))
		  (+ (stats-num-primsubs (mating-stats mating1))
		     (stats-num-primsubs (mating-stats mating2))))
	    (setf (stats-mate-subsume-tests (mating-stats active-mating))
		  (+ (stats-mate-subsume-tests (mating-stats mating1))
		     (stats-mate-subsume-tests (mating-stats mating2))))
	    (setf (stats-mate-subsume-true (mating-stats active-mating))
		  (+ (stats-mate-subsume-true (mating-stats mating1))
		     (stats-mate-subsume-true (mating-stats mating2))))
	    (setf (stats-unif-subsume-tests (mating-stats active-mating))
		  (+ (stats-unif-subsume-tests (mating-stats mating1))
		     (stats-unif-subsume-tests (mating-stats mating2))))
	    (setf (stats-unif-subsume-true (mating-stats active-mating))
		  (+ (stats-unif-subsume-true (mating-stats mating1))
		     (stats-unif-subsume-true (mating-stats mating2))))
	    (setf (stats-time-used (mating-stats active-mating))
		  (cons (+ (car (stats-time-used (mating-stats mating1)))
			   (car (stats-time-used (mating-stats mating2)))) 0))
	    (setf (mating-completep active-mating) t)
	    active-mating)
	(progn (setf (mating-completep mating1) nil) mating1))
    (progn (when mating2
		 (setf (mating-completep mating2) nil)
		 mating2))))

(defun ms88-controller (&optional
				 (etree (eproof-etree current-eproof)))
  (let ((mating (if ms-split (ms-top etree)
		    (setq active-mating (ms-director etree dup-allowed)))))
    (when (and mating active-mating (mating-clist active-mating))
      (msg t "Changing active mating."))
    (setq active-mating mating)))
