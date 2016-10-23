;; -*- Mode:LISP; Package:auto -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;
(in-package :AUTO)

;;;
;;; File: MTREE-TOP
;;; Package: MS88
;;;
;;; Defines the matingstree toplevel as well as ...

(deffile mtree-unification
  (part-of mst)
  (extension lsp)
  (mhelp "Defines unification as used in matingstree."))

;was defmacro, which seems odd
#+comment(defun mst-subst-stack ()
   (if first-order-mode-ms
       (mating-utree (matingstree-mating current-matingstree))
       (node-subst-stack (car (mating-success-nodes 
                                  (matingstree-mating current-matingstree))))))

(defun mst-subst-stack ()
   (if first-order-mode-ms
       (mating-utree (matingstree-mating current-matingstree))
     (let ((a-mating (matingstree-mating current-matingstree)))
       (multiple-value-bind (flag root subst-hash-table success-nodes)
			    (unifiable-p a-mating T)
			    (declare (ignore flag root))
			    (if success-nodes 
				(node-subst-stack (car success-nodes))
			      nil)))))

(defun mst-quick-unification-connection (connection subst-stack)
  (if first-order-mode-ms
      (mst-fo-unify-conn
	connection (free-vars-in-etree current-eproof) subst-stack)
      (quick-unification-main (list (connection-to-dpair-main connection)))))

(defun mst-fo-unify-conn (connection free-vars subst-stack)
  (multiple-value-bind (head1 args1 pos1)
      (fo-hnf (not (jform-pos (car connection)))
	      (jform-represents (car connection)) nil nil)
    (multiple-value-bind (head2 args2 pos2)
	(fo-hnf (jform-pos (cdr connection))
		(jform-represents (cdr connection)) nil nil)
      (if (and (eq pos1 pos2) (eq head1 head2))
	  (fo-unify (pairlis args1 args2) subst-stack free-vars)
	  'fail))))


(defun mst-check-potential-connection (key connection cgraph clist subst-stack)
  (multiple-value-bind (value occursp)
      (gethash key cgraph)
      (if occursp
	  (progn
	    (signal-event 'considered-conn (or value key))
	    (if (and value 
		   (or (infinite-p max-mates) 
                       (not (maxmates-fail-message 
                                 (option-> (connections-counter (cons value clist) 
                                                  (connections-array)) max-mates))))
		   (not (subsumed-clist-p value (cons value clist)
					  (incomp-clists-wrt-etree)))
		   (not (subsumed-clist-p value (cons value clist)
					  (incomp-clists))))
	      value nil))
;the trouble with the above is that connections that fail on one branch 
;might get entered into the cgraph as useless, and so can't be used on
;another branch. This over-fixes the problem and makes the cgraph almost
;worthless --- should find some other way to fix it. 
	(if (or mate-ffpair (rigidnode-p (car connection))
		(rigidnode-p (cdr connection)))
	    (progn (if (multiple-value-bind (unifiablep dpairs subst)
			     (mst-quick-unification-connection connection subst-stack)
			     (insert-in-cgraph key connection 
                                               unifiablep 
			     		       (nconc subst dpairs) cgraph)
			     )
		       (progn (let ((value2 (gethash key cgraph)))
				(if (maxmates-fail-message 
				     (option-> (connections-counter (cons value2 clist)
                                                           (connections-array)) max-mates))
				    nil value2)))
		     nil))
	    (progn 
	      (signal-event 'considered-conn key) nil)))))

(defun eligible-literal (lit1 ob1 lit2 ob2)
  (let* (;(last-jform (eproof-jform current-eproof))
	 (pair (update-current-etree-2 lit1 ob1 lit2 ob2))
	 (jform (eproof-jform current-eproof))
	 (tlit1 (find-jform-name (car pair) jform))
	 (tlit2 (find-jform-name (cdr pair) jform))
	 ;these should not be nil. I can't work out why they sometimes are. MB.
	 (a-mating 
	  (hxcopy-mating (matingstree-mating current-matingstree)))
	 (mate-ffpair t)
	 (connection (if (or (null tlit1) (null tlit2)) nil 
		       (find-potential-connection
			  tlit1 (list tlit2) (cgraph)
			  (mating-clist a-mating)))))
    (setq connection (or connection
			 (when (not (or reduce-double-neg first-order-mode-ms))
			       (and tlit1 tlit2
			       (find-potential-connection
				tlit2 (list tlit1) (cgraph)
				(mating-clist a-mating))))))
;     (setf (eproof-jform current-eproof) last-jform)
;above line makes logical sense, but needs something else or it crashes...
    connection))
