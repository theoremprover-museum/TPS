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

(deffile connections
;  (part-of mating-search)
  (part-of ms88)
  (mhelp "Functions to find connections in a ETREE."))

(defun find-potential-connection (literal path cgraph clist &optional (start-time nil) (time-limit nil))
  "Returns the next potential CONNECTION on PATH which does not occur in
  INCOM-CONNS.  Consults and UPDATES CGRAPH to find whether a suggested
  connection is (potentially) unifiable. Returns NIL if there's no new
  connection."
  (dolist (second path nil)
    (let ((name (check-potential-connection
		  (cons (literal-name literal) (literal-name second))
		  (cons literal second) cgraph clist start-time time-limit)))
      (if name (return name)
	  (when (not (or reduce-double-neg first-order-mode-ms))
	    (setq name (check-potential-connection
			 (cons (literal-name second) (literal-name literal))
			 (cons second literal) cgraph clist start-time time-limit))
	    (if name (return name)))))))

(defun check-potential-connection (key connection cgraph clist &optional (start-time nil) (time-limit nil))
  "First checks whether an entry for CONNECTION occurs in the CGRAPH. If
  not, computes the entry, UPDATES the CGRAPH, and returns whether the
  connection is unifiable. If yes and the connection is potentially unifiable,
  returns T if adding the CONNECTION to CLIST does not make it a superset of
  a known INCOMP-CLIST. Else, returns NIL."
  (declare (special max-mates))
  (multiple-value-bind (value occursp)
      (gethash key cgraph)
    (if occursp
	(progn
	  (signal-event 'considered-conn (or value key))
	  (if (and value 
		   (or (infinite-p max-mates) 
;;;if a connection occurs more than once, whether should it be counted more than once?
                       (not (maxmates-fail-message 
                                 (option-> (connections-counter (cons value clist) 
                                                  (connections-array)) max-mates))))
		   (not (subsumed-clist-p value (cons value clist)
					  (incomp-clists-wrt-etree)))
		   (not (subsumed-clist-p value (cons value clist)
					  (incomp-clists))))
	      value nil))
	(if (or mate-ffpair (rigidnode-p (car connection))
		(rigidnode-p (cdr connection)))
	    (progn (if (multiple-value-bind (unifiablep dpairs subst)
					(quick-unification-connection connection start-time time-limit)
					(insert-in-cgraph key connection 
                                               unifiablep (nconc subst dpairs) cgraph))
		       (progn (let ((value2 (gethash key cgraph)))
				(if (maxmates-fail-message 
				     (option-> (connections-counter (cons value2 clist)
                                                           (connections-array)) max-mates))
				    nil value2)))
		     nil))
	    (progn 
	      (signal-event 'considered-conn key) nil)))))

(defun locate-connection-on-path (path cgraph clist)
  (do ((path path (cdr path)))
      ((null (cdr path)) nil)
    (let ((name (find-potential-connection
		  (car path) (cdr path) cgraph clist)))
      (when name (return name)))))


(defun locate-connection-on-mspath (path cgraph clist)
  (when path
    (multiple-value-bind (name newpath closed)
	(locate-connection-on-mspath (cdr path) cgraph clist)
      (if (or name closed) (values name newpath closed)
	  (if (open-mspath-p (car path) (cdr path) cgraph clist)
	      (let ((name (find-potential-connection
			    (car path) (cdr path) cgraph clist)))
		(when name (values name path)))
	      (values nil path t))))))

(defun open-mspath-p (literal path cgraph clist)
  "Returns NIL if adding LITERAL to PATH closes the PATH, else returns T."
  (let ((first (literal-name literal)))
    (dolist (second path t)
      (let ((name (gethash (cons first (literal-name second)) cgraph)))
	(if (or (and name (memq name clist))
		(let ((name (gethash (cons (literal-name second) first)
				     cgraph)))
		  (and name (memq name clist))))
	    (return nil))))))

(defun maxmates-fail-message (bool)
  (if bool 
      (case mating-verbose 
	    (max (msgf "Connection rejected because MAX-MATES is too low."))
	    ((min med) (msg "M"))))
  bool)


(defun connections-counter (mlist carray)
  (let ((matinglist (connection-flattenlist mlist carray)))
    (count-conns matinglist 0)))

(defun count-conns (m p)
  (if (null m) p
    (count-conns (cdr m) (max p (count-occs (car m) (cdr m) 1)))))

(defun count-occs (a b c)
  (if (null b) c
    (if (string-equal a (car b)) (count-occs a (cdr b) (1+ c)) (count-occs a (cdr b) c)))) 

(defun connection-flattenlist (m c)
  (if (null m) nil
    (append 
     (if (consp (car m)) ; cebrown 2/16/01 - to fix a bug
	 (list (princ-to-string (caar m)) (princ-to-string (cdar m)))	; cebrown 2/16/01
       (list (princ-to-string (literal-name (car (car (gethash (car m) c)))))
	     (princ-to-string (literal-name (cdr (car (gethash (car m) c)))))))
     (connection-flattenlist (cdr m) c))))


