;;; -*- Mode:LISP; Package:auto -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package "AUTO")
;(part-of mating-search)
(part-of ms88)

(deffile mating-aux
;  (part-of mating-search)
  (part-of ms88)
  (mhelp "Auxiliary functions used by the mating search package."))

(defun init-mating (&optional (clist nil) (first-path nil))
  "Initializes a mating."
  (let ((new-mat 
	  (make-mating :clist clist
	       :utree (if first-order-mode-ms
			  (multiple-value-bind (flag dpairs)
			      (first-order-unification (clist-to-dpairs clist))
			    (declare (ignore flag))
			    dpairs)
			  (clist-to-dpairs clist))
	       :first-path first-path)))
    (setf (symbol-value (mating-name new-mat)) new-mat)
    new-mat))

(defun insert-in-cgraph (key connection unifiablep value cgraph)
  "If the VALUE is T, then associates a number with the KEY in the CGRAPH.
  Returns the number, if any, associated with the CONNECTION."
  (if (eq unifiablep 'fail)
      (progn (setf (gethash key cgraph) nil) nil)
      (progn (incf (max-cgraph-counter))
	     (let ((counter (max-cgraph-counter)))
	       (signal-event 'considered-conn counter)
	       (setf (gethash key cgraph) counter)
	       (setf (gethash counter (connections-array))
		     (cons connection value))
	       (when (or reduce-double-neg first-order-mode-ms)
		 (setf (gethash (cons (cdr key) (car key)) cgraph) counter))
	       counter))))

(defun subsumed-clist-p (connection clist incomp-clists)
  "For each list in INCOMP-CLISTS which contains CONNECTION checks whether
  CLIST is a superset."
  (dolist (incomp-clist incomp-clists nil)
    (if (and (member connection incomp-clist :test #'=)
	     (subsetp incomp-clist clist :test #'=))
	(return T))))

(defun subsumed-p (clist incomp-clists)
  "For each list in INCOMP-CLISTS checks whether CLIST is a superset."
  (dolist (incomp-clist incomp-clists nil)
    (if (subsetp incomp-clist clist :test #'=)
	(return T))))

(defun find-active-mating (mating-list)
  (if (option-> (bktrack-limit) (mating-bktrack (car mating-list))) 
; option-> is from the ms91 procedure; it is the > function for things
; of type integer+-or-infinity. (Changed 5/19/93 MB)
      (values (car mating-list) mating-list nil)
      (if (and (cadr mating-list)
	       (option-> (bktrack-limit) (mating-bktrack (cadr mating-list))))
	  (let ((elt (pop mating-list)))
	    (values (car mating-list) (nconc mating-list (list elt)) T))
	(dolist (elt mating-list
		     (progn
		       (if (integerp (bktrack-limit)) (incf (bktrack-limit)))
		       (if (or (not duplicated) new-mating-after-dup)
			   (let ((new (init-mating)))
			     (values new (cons new mating-list) T))
			 (values (car mating-list) mating-list nil))))
	  (setf (mating-bktrack elt) 0)))))


(defun print-connection (connection connections-array)
  (let ((conn (car (gethash connection connections-array))))
    (msg (cons (show-prop-name (car conn)) (show-prop-name (cdr conn))) 2)))

(defun print-clist (clist connections-array)
  (msg t)
  (let ((counter 0))
    (dolist (connection clist)
      (when (= counter 3)
	(terpri)
	(setq counter 0))
      (incf counter)
      (print-connection connection connections-array))))

(defun insert-conn-in-utree (clist utree)
  "Inserts the list of connections CLIST at the leaves of the UTREE, and
  returns the modified UTREE."
  (if (node-p utree)
      (insert-conn-in-utree-rec clist utree 0)
      (nconc (clist-to-dpairs clist) utree)))

(defun insert-conn-in-utree-rec (clist utree depth)
  (if (node-sons utree)
      (dolist (son (node-sons utree) (funcall update-measure-int-node utree))
	(insert-conn-in-utree-rec clist son (1+ depth)))
      (unless (eq (node-terminal-flag utree) 'fail)
	(setf (node-terminal-flag utree) nil)
	(setf (node-old-dpairs utree) (node-dpairs utree))
	(setf (node-dpairs utree)
	      (nconc (clist-to-dpairs clist) (node-old-dpairs utree)))
	(let ((parent (node-parent utree)))
	  (if parent
	      (funcall assign-measure (list utree)
		       (node-measure parent) (length (node-sons parent))
		       depth)
	      (setf (node-measure utree) initial-measure-root)))
	(setf (node-connections utree)
	      (append clist (node-connections utree)))))
  utree)

(defun find-all-connections-in-utree (utree)
  (append (node-connections utree)
	  (delete-duplicates
	    (mapcan #'find-all-connections-in-utree (node-sons utree))
	    :test #'=)))

(defun locate-connection-in-utree (connection utree)
  (if (member connection (node-connections utree) :test #'=) (list utree)
      (mapcan #'(lambda (son) (locate-connection-in-utree connection son))
	      (node-sons utree))))

(defun remove-conn-from-utree (connection utree)
  "Removes the CONNECTION from the UTREE, and returns the modified UTREE.
  Basically, purges the entire unification tree below the nodes where the
  connection first occurred, and moves all connections under that node
  to that node."
  (dolist (node (locate-connection-in-utree connection utree))
    (setf (node-connections node) (find-all-connections-in-utree node))
    (setf (node-dpairs node)
	  (nconc (mapcar #'connection-to-dpair (node-connections node))
		 (node-old-dpairs node)))
    (setf (node-sons node) nil)))

(defun first-order-problem-p (vars)
  (and (not ALLOW-NONLEAF-CONNS) ; cebrown 2/17/01 - need HOU to mate nonleaves
       (dolist (var vars T)
	 (unless (first-order-var (car var)) (return nil)))))

(defun rigidnode-p (node)
  (not (member (head (jform-represents node))
               (free-vars-in-etree master-eproof))))


(defun find-all-nodes-in-clist (clist)
  (let ((nodelist nil)
	(carray (connections-array)))
    (dolist (connection clist (sort (delete-duplicates
				      nodelist :test #'string=) 
				    #'string< ))
      (let ((conn (car (gethash connection carray))))
	(push (literal-name (car conn)) nodelist)
	(push (literal-name (cdr conn)) nodelist)))))

(defun initialize-ms-after-dup ()
  (if (and new-mating-after-dup duplicated)
      (push (init-mating) (mating-list))
      (let ((mating-list (mating-list)))
	(dolist (clist (find-maximal-clists (max-incomp-clists-wrt-etree))
		       (setf (mating-list)
			     (or mating-list (list (init-mating)))))
	  (push (init-mating clist) mating-list))))
  (setf (incomp-clists-wrt-etree) nil)
  (setf (max-incomp-clists-wrt-etree) nil))

(defun find-maximal-clists (clists)
  (setq clists (sort clists #'< :key #'length ))
  (do ((old-clists clists (cdr old-clists)))
      ((null old-clists))
    (let ((first (car old-clists)))
      (dolist (second (cdr old-clists))
	(when (subsetp first second)
	  (setq clists (delete first clists))
	  (return)))))
  clists)

(defflag primsub-var-select
  (flagtype boolean)
  (default t)
  (subjects primsubs transmit)
  (change-fn (lambda (flag newvalue oldvalue)
	       (declare (ignore flag))
	       (unless (equal oldvalue newvalue)
		 (clrhash prim-hashtable))))
  (mhelp "If T, primsubs will only be applied to those variables
which occur both negatively and positively as the head variable
of some leaves in the current eproof.
If NIL, primsubs will be applied to any variable which occurs
either negatively or positively or both, anywhere"))

(defun apply-prim-subst-for-var (var)
  "Returns T if VAR occurs both positively and negatively as head var
       in leaves of CURRENT-EPROOF."
  (if primsub-var-select
      (member (exp-var-var var)
	      (intersection 
	       (mapcar #'(lambda (x) (head (auto::get-shallow x))) (remove-if-not #'positive-p (find-etree-nodes #'leaf-p)))
	       (mapcar #'(lambda (x) (head (auto::get-shallow x))) (remove-if #'positive-p (find-etree-nodes #'leaf-p))))
	      :test 'equal)
    t))
  
(defun occurs-on-path-p (connection next-open-path)
  (let ((connection (car (gethash connection (connections-array)))))
    (and (member (car connection) next-open-path :test #'eq)
	 (member (cdr connection) next-open-path :test #'eq))))

(defun modify-utree (connection mating)
  (let ((utree (mating-utree mating)))
    (unless (and first-order-mode-ms
		 (dolist (dpair (cdr (gethash connection (connections-array)))
				(progn (setf (mating-utree mating) utree)
				       t))
		   (if (eq dpair (car utree)) (setq utree (cdr utree))
		       (return nil))))
      (setf (mating-utree mating)
	    (if first-order-mode-ms
		(multiple-value-bind (flag dpairs)
		    (first-order-unification
		      (clist-to-dpairs (mating-clist mating)))
		  (declare (ignore flag))
		  dpairs)
		(clist-to-dpairs (mating-clist mating))))
      (setf (mating-subst-hash-table active-mating) nil))))

(defun find-substs-for-mating (mating)
  "Returns the subst-stack associated with the MATING." 
  (if (listp (mating-utree mating))
      (mating-utree mating)
      (let ((success-node (or (car (mating-success-nodes mating))
			      (find-success-node-in-utree
				(mating-utree mating)))))
	(if success-node (node-subst-stack success-node) nil))))

(context etr-nat)

(defflag remove-leibniz
  (flagtype boolean)
  (default t)
  (subjects mating-search etrees etr-nat ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 transmit)
  (mhelp "If TRUE, selection parameters corresponding to Leibniz equality
definitions will be removed from expansion proofs during merging 
(cf. Pfenning's thesis, theorem 138)."))

(defflag merge-minimize-mating
  (flagtype boolean)
  (default t)
  (subjects mating-search etrees etr-nat ms88 ms89 ms90-3 ms90-9 ms91-6 ms91-7 ms92-9 ms93-1 ms98-1 transmit)
  (mhelp "If T, merging will attempt to minimize the mating
by removing any unnecessary connections. If NIL, it won't.
T will sometimes produce a more readable ND proof, but can
also take a very long time."))
