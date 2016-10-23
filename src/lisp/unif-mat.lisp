;;; -*- Mode:LISP; Package:auto -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package "AUTO")
(part-of ms88)

(deffile unif-mat
  (part-of ms88)
  (mhelp "Interface between mating search and unification."))

(defun clist-to-dpairs (clist)
  (apply-fn-with-key 'nconc clist 'connection-to-dpair))

(defun copy-dpair (dpair)
  (if (or (uni-term-p (car dpair)) (uni-term-p (cdr dpair)))
      (cons (if (uni-term-p (car dpair))
		(copy-uni-term (car dpair)) (car dpair))
	    (if (uni-term-p (cdr dpair))
		(copy-uni-term (cdr dpair)) (cdr dpair)))
      dpair))

(defun connection-to-dpair (connection)
  (if first-order-mode-ms
      (copy (cdr (gethash connection (connections-array))))
      (mapcar #'copy-dpair (cdr (gethash connection (connections-array))))))

(defun connection-to-dpair-main (connection)
  (let ((first (car connection))
	(second (cdr connection)))
    (if (jform-pos first)
	(if (jform-pos second)
	    (cons (make-uni-term :head (jform-represents second))
		  (make-uni-term :head (jform-represents first) :pos nil))
	    (cons (make-uni-term :head (jform-represents first))
		  (make-uni-term :head (jform-represents second))))
	(if (jform-pos second)
	    (cons (make-uni-term :head (jform-represents first))
		  (make-uni-term :head (jform-represents second)))
	    (cons (make-uni-term :head (jform-represents first))
		  (make-uni-term :head (jform-represents second)
				 :pos nil))))))
(defvar ms88-unif-counter 0)

#+comment(defun banned-conn-p (connection)
  (let ((conn (cons (literal-name (car connection)) (literal-name (cdr connection)))))
    (when conn
	  (or (member conn *banned-conns-list* :test 'equal)
	      (and (setq conn (cons (cdr conn) (car conn)))
		   (member conn *banned-conns-list* :test 'equal))))))

(defun banned-conn-p (conn)
  (or (member conn *banned-conns-list* :test 'equal)
      (and (setq conn (cons (cdr conn) (car conn)))
	   (member conn *banned-conns-list* :test 'equal))))

(defun quick-unification-connection (connection &optional (start-time nil) (time-limit nil))
  (when (and *banned-conns-list* (banned-conn-p connection))
	(when (memq mating-verbose '(med max)) (msg "B"))
	(return-from quick-unification-connection 'fail))
  (if first-order-mode-ms
      (fo-unify-conn
	connection (free-vars-in-etree current-eproof))
      (quick-unification-main (list (connection-to-dpair-main connection)) start-time time-limit)))

(defun quick-unification-main (dpairs &optional (start-time nil) (time-limit nil))
  (let ((free-vars (free-vars-in-etree current-eproof))
	(stop-at-tsn t))
    (declare (special stop-at-tsn))
    (if first-order-mode-ms (fo-unify dpairs nil free-vars)
	(multiple-value-bind (flag subst-stack dpairs)
	    (simpl dpairs nil free-vars 0)
	  (if (eq flag 'fail) 'fail
	      (if (eq flag 'success)
		  (values 'more subst-stack dpairs)
		  (if (eq flag 'more)
		      (multiple-value-bind (root-node subst-hash-table)
			  (initialize-utree
			    (mapcar #'copy-dpair dpairs) free-vars subst-stack)
			(multiple-value-bind
			    (flag root hash-table success-nodes)
			    (unify root-node subst-hash-table 99 start-time time-limit)
			  (declare (ignore root hash-table success-nodes))
			  (values flag dpairs subst-stack)))
		      (throwfail))))))))

(defun unifiable-p (mating &optional (quickp nil) (start-time nil) (time-limit nil))
  (let ((max-utree-depth (mating-utree-depth mating))
	(stop-at-tsn t))
    (declare (special max-utree-depth))
    (multiple-value-prog1
     (if (node-p (mating-utree mating))
         (unify (mating-utree mating) (mating-subst-hash-table mating)
                quickp start-time time-limit)
         (multiple-value-bind (root-node subst-hash-table)
             (initialize-utree
              (mating-utree mating)
              (free-vars-in-etree current-eproof))
           (unify root-node subst-hash-table quickp start-time time-limit))))))

(defun first-order-unification
  (dpairs &optional (free-vars (free-vars-in-etree current-eproof)))
  (fo-unify dpairs nil free-vars))

(defun fo-compatible-p (conn substs &optional (free-vars nil flag))
  (unless flag
    (setq free-vars (free-vars-in-etree current-eproof)))
  (fo-unify (connection-to-dpair conn) substs free-vars))
