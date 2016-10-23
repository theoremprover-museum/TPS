(in-package :auto)

;;;pair is a pair of literals in jform.
(defun natree-node-in-mating (name mating)
  (let ((conn (connections-array)))
    (dolist (num (mating-clist mating))
       (let ((pair (car (gethash num conn))))
	 (if (or (eq name (literal-name (car pair)))
		 (eq name (literal-name (cdr pair))))
	     (return T))))))

(defun natree-cleanup-reverse-rewrites (etree)
  (if (and (rewrite-p etree) (rewrite-reverse etree))
      (let ((newrew (make-rewrite :shallow (get-shallow (first (etree-components etree)))
				  :positive (etree-positive etree)
				  :free-vars (etree-free-vars etree)
				  :junctive 'NEUTRAL
				  :components (list etree)
				  :parent (etree-parent etree)
				  :justification 'EQUIVWFFS))
	    (parent (etree-parent etree)))
	(setf (etree-parent etree) newrew)
	(if parent (rplaca (member etree (etree-components parent)) newrew))))
  (mapc #'natree-cleanup-reverse-rewrites (etree-components etree)))

(defun natree-cleanup-inner-nodes (etree)
  (if (etree-leaf etree)
      (cond ((not (natree-node-in-mating (etree-name etree) active-mating))
	     (setf (etree-leaf etree) nil))
	    ((rewrite-p etree)
	     (if (find-etree-node #'(lambda (node)
					  (natree-node-in-mating (etree-name node)
								 active-mating))
				  (first (etree-components etree)))
		 (msgf "This case has to be worked out yet!!!")
	       (let ((new-leaf (create-leaf-node (rewrite-shallow etree)
						 (etree-positive etree)
						 (etree-free-vars etree)
						 (etree-parent etree)))
		     (parent (etree-parent etree)))
		 (if parent (rplaca (member etree (etree-components parent)) new-leaf)))))
	    (otherwise))) ;;;otherwise, we have to discuss these cases individually.
    (mapc #'natree-cleanup-inner-nodes (etree-components etree)))

#+comment
(defun etree-with-shallow (etree)
  (or (leaf-p etree) (rewrite-p etree)
      (selection-p etree) (skolem-p etree) (expansion-p etree)))

#+comment
(defun natree-cleanup-inner-nodes (etree)
  (if (and (etree-leaf etree)
	   (not (natree-node-in-mating (etree-name etree) active-mating)))
      (setf (etree-leaf etree) nil))
  (mapc #'natree-cleanup-inner-nodes (etree-components etree)))

#+comment
(defun empty-dup-info-to-leaf (etree)
  (let ((new-leaf (create-leaf-node (empty-dup-info-shallow etree)
				    (etree-positive etree)
				    (etree-free-vars etree)
				    (etree-parent etree)))
	(parent (etree-parent etree)))
    (if parent (rplaca (member etree (etree-components parent)) new-leaf))))

#+comment
(defun replace-empty-dup-infos (etree)
  (if (empty-dup-info-p etree)
      (empty-dup-info-to-leaf etree)
    (mapc #'replace-empty-dup-infos (etree-components etree))))

(defun natree-precleanup ()
  (natree-cleanup-reverse-rewrites current-topnode)
  (natree-cleanup-inner-nodes current-topnode)
  (cleanup-all-inactive-nodes  current-topnode)
  (elim-lambda-rew current-topnode)
  #+comment ;;;This does not work since we are using selection nodes, NOT skolem nodes.
  (min-quant-etree-app current-topnode)
  (add-lambda-rew current-topnode))

#+comment
(defun ml::ugen-legal (p2 p1 a |x| p2-hyps p1-hyps)
  (declare (ignore p2 p1 a |x| p2-hyps p1-hyps)) T)