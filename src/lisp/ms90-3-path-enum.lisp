;;; -*- Mode:LISP; Package:auto -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1990 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;


(in-package :AUTO)
(part-of ms90-3)

(deffile ms90-3-path-enum
  (part-of ms90-3)
  (mhelp "Path enumerator used in the implementation of Path-focused
duplication.")) 

;;; A path can consist of the following elements:
;;; 1. Literal 2. Universal Jform 3. ENV to indicate that till the
;;; next ENV is encountered, the literals on the path are in the scope
;;; of this ENV.

(eval-when (compile load eval)

(defmacro empty-conjunction (jform)
   `(and (conjunction-p ,jform) (null (conjunction-components ,jform))))

(defmacro empty-disjunction (jform)
   `(and (disjunction-p ,jform) (null (disjunction-components ,jform))))
)

(defun car-not-empty (components)
  (dolist (com components)
    (if (not (empty-conjunction com)) (return com))))

(defun pair-in-mating-p (lit1 env1 lit2 env2 mating)
    (do ((mating mating (cddr mating)))
	((null mating) nil)
	(let ((conn (car mating))
	      (indices (cadr mating)))
	  (if (and (eq (car-conn conn) lit1) (= (car indices) (cdar env1))
                   (eq (cdr-conn conn) lit2) (= (cdr indices) (cdar env2)))
              (return T)))))

(defun find-cheapest-path (jform path open-path unif-prob env-stack dup-record exp-index mating 
				 &optional (start-time nil) (time-limit nil))
  (case (jform-type jform)
    (literal
     (check-conn jform path open-path unif-prob env-stack dup-record
		       exp-index mating start-time time-limit))
    (disjunction
     (if (empty-disjunction jform)
         (values t (cons jform path) unif-prob env-stack
		   dup-record exp-index mating)
         (find-cheapest-path
           (car (disjunction-components jform)) path open-path unif-prob env-stack
           dup-record exp-index mating start-time time-limit)))
    (conjunction
     (let (env conn-added-p)
       (dolist (conj (conjunction-components jform)
		     (values nil path unif-prob env-stack dup-record exp-index
			     mating))
	 (multiple-value-setq
	     (conn-added-p path unif-prob env dup-record exp-index
			   mating)
	     (find-cheapest-path conj path open-path unif-prob env-stack
				 dup-record exp-index mating start-time time-limit))
	 (if conn-added-p
	     (return (values conn-added-p path unif-prob env dup-record
			     exp-index mating))
	     (unless (eq env env-stack)
	       (setq path (cons env path)))))))
    (universal
     (let ((old-env env-stack))
       (dolist (var (universal-qvars jform) (incf exp-index))
	 (push (cons var exp-index) env-stack)
	 (push (car env-stack) dup-record))
       (find-cheapest-path
	(universal-scope jform) (cons jform (cons old-env path)) open-path
	unif-prob env-stack dup-record exp-index mating start-time time-limit)))
    (t (throwfail "No action specified for " (jform-type jform)))))


(defun find-alt-path-top (last-elt rem-path open-path unif-prob env-stack
				   dup-record exp-index mating
				   touchedp conjuncts &optional (start-time nil) (time-limit nil))
  (if (consp last-elt)
      (find-alt-path-top (car rem-path) (cdr rem-path) open-path unif-prob
			 last-elt dup-record exp-index mating
			 touchedp conjuncts start-time time-limit)
      (find-alt-path last-elt rem-path open-path unif-prob env-stack
		     dup-record exp-index mating
		     (or touchedp (lit-in-mating-p
				   last-elt rem-path mating)) 
		     conjuncts start-time time-limit)))

(defun find-alt-path (last-elt rem-path open-path unif-prob env-stack
			       dup-record exp-index mating touchedp conjuncts &optional (start-time nil) (time-limit nil))
  (declare (inline car-not-empty))
  (let ((parent (jform-parent last-elt)))
    (if parent
	(case (jform-type parent)
	  (universal
	   ;;The remaining path must begin with UNIVERSAL JFORM
	   ;;followed by ENV-STACK.
	   (find-alt-path
	    parent (cddr rem-path) open-path unif-prob (cadr rem-path)
	    (cons (universal-qvars (car rem-path)) dup-record)
	    exp-index mating touchedp conjuncts start-time time-limit))
	  (disjunction
	   (let ((disj (cadr (memq last-elt (disjunction-components parent)))))
	     (if (and disj (or touchedp (jform-mark last-elt))) 
                 ;;;using the mark of the universal jform to fix a bug(pell26)
		 (multiple-value-bind
		     (conn-added-p path unif-prob env-stack dup-record
				   exp-index mating)
		     (find-cheapest-path
		      disj rem-path open-path unif-prob env-stack dup-record
		      exp-index mating start-time time-limit)
		   (if conn-added-p (values conn-added-p path unif-prob
					    env-stack dup-record
					    exp-index mating)
		       (complete-alt-path
			parent path open-path unif-prob env-stack dup-record
			exp-index mating start-time time-limit)))
                 (if (or touchedp (eq last-elt (car (disjunction-components parent))))
                     (find-alt-path parent rem-path open-path unif-prob
                                    env-stack
                                    (if touchedp
                                        dup-record
                                        (acons parent bkup-disj dup-record))
                                    exp-index mating touchedp conjuncts start-time time-limit)
                     (values nil 'alt-conn unif-prob env-stack dup-record
                             exp-index mating)))))
	  (conjunction
	   (if (eq (car-not-empty (conjunction-components parent)) last-elt)
	       (find-alt-path
		parent rem-path open-path unif-prob env-stack dup-record
		exp-index mating
		(or touchedp (jform-mark last-elt) (memq parent conjuncts)) conjuncts start-time time-limit)
	       (find-alt-path-top
		(car rem-path) (cdr rem-path) open-path unif-prob env-stack
		dup-record exp-index mating nil
		(if  (or touchedp (jform-mark last-elt))  ;;;using the mark to fix a bug
                      (pushnew parent conjuncts) conjuncts) start-time time-limit))))
	(values nil nil unif-prob env-stack dup-record exp-index mating
		touchedp))))

(defun complete-alt-path (jform path open-path unif-prob env-stack dup-record
				exp-index mating &optional (start-time nil) (time-limit nil))
  (let ((parent (jform-parent jform)))
    (if parent
	(case (jform-type parent)
	  (conjunction
	   (complete-alt-path-and
	    parent jform path open-path unif-prob env-stack dup-record
	    exp-index mating start-time time-limit))
	  (disjunction (complete-alt-path
			parent path open-path unif-prob env-stack dup-record
			exp-index mating start-time time-limit))
	  (universal (complete-alt-path
		      parent
		      (if (consp (car path)) path (cons env-stack path))
		      open-path unif-prob
		      (cadr (memq parent path)) dup-record exp-index mating start-time time-limit)))
	(values nil path unif-prob env-stack dup-record exp-index mating))))

(defun complete-alt-path-and (parent jform path open-path unif-prob env-stack
				     dup-record exp-index mating &optional (start-time nil) (time-limit nil))
  (let (conn-added-p env)
    (dolist (conj (cdr (memq jform (conjunction-components parent)))
		  (complete-alt-path parent path open-path unif-prob env-stack
				     dup-record exp-index mating start-time time-limit))
      (multiple-value-setq
	  (conn-added-p path unif-prob env dup-record exp-index mating)
	  (find-cheapest-path conj path open-path unif-prob env-stack
			      dup-record exp-index mating start-time time-limit))
      (if conn-added-p
	  (return (values conn-added-p path unif-prob env dup-record exp-index
			  mating))
	  (unless (eq env env-stack) (setq path (cons env path)))))))


(defun check-conn (literal path open-path unif-prob env-stack dup-record
			   exp-index mating &optional (start-time nil) (time-limit nil))
      (multiple-value-bind (lit2 index unif-prob env2 subpath)
          (check-conn-path literal env-stack path env-stack unif-prob mating start-time time-limit)
        (if lit2
            (let* ((newpath (cons literal path))
                   (conn (make-conn :lit1 literal :lit2 lit2
                                    :old-env env-stack
                                    :env env2
                                    :path newpath
                                    :subpath subpath)))
	      (when monitorflag (funcall (symbol-function *current-monitorfn*) 'added-conn 
					 (list (cons 'mating 
						     (cons conn 
							   (acons (cdar env-stack) index mating))) 
					       (cons 'npfd nil))))
	      (values t newpath unif-prob env-stack
                      (cons conn dup-record) exp-index
                      (cons conn (acons (cdar env-stack) index mating))))
            (multiple-value-bind (lit2 index unif-prob env2 subpath)
                (check-conn-path literal env-stack open-path env2 unif-prob mating start-time time-limit)
              (if lit2
                  (let* ((newpath (cons literal path))
                         (conn (make-conn :lit1 literal :lit2 lit2
                                          :old-env env-stack
                                          :env env2
                                          :path newpath
                                          :subpath subpath))) 
		    (when monitorflag (funcall (symbol-function *current-monitorfn*) 'added-conn 
					       (list (cons 'mating 
							   (cons conn 
								 (acons (cdar env-stack) index mating))))))
		    (values t newpath unif-prob env-stack
                            (cons conn dup-record) exp-index
                            (cons conn (acons (cdar env-stack) index mating))))
                  (values nil (cons literal path) unif-prob env-stack
                          dup-record exp-index mating))))))


(defun initialize-conn (lit1 env1 lit2 env2)
  (cons (ho-unif-lnorm
         nil (ho-bind-vars (if (jform-pos lit1)
                               (cons 'not (jform-represents lit1))
                               (jform-represents lit1))
                           env1 nil)
         nil nil nil)
        (ho-unif-lnorm
         nil (ho-bind-vars (if (jform-pos lit2) (jform-represents lit2)
                               (cons 'not (jform-represents lit2)))
                           env2 nil)
         nil nil nil)))

(defun check-conn-path (lit1 env1 path env2 unif-prob mating &optional (start-time nil) (time-limit nil))
  (dolist (lit2 path (values nil nil unif-prob env2))
    (if (consp lit2) (setq env2 lit2)
	(unless (eq (jform-type lit2) 'universal) 
           (if (pair-in-mating-p lit1 env1 lit2 env2 mating)
               (return (values lit2 (cdar env2) unif-prob nil ;;;(conn-env (car mating))
                               (cdr (path-to-subpath (member lit2 path))))))
           (if (and (> (find-paths-below lit1) 0) 
		    (> (find-paths-below lit2) 0))
              (multiple-value-bind (new-unif-prob conn)
                  (conn-unif-p lit1 env1 lit2 env2 unif-prob start-time time-limit)
                (when new-unif-prob
                  (when (eq mating-verbose 'max)
                    (msg t "Adding connection: " lit1 " . " lit2 4
                         (cdar env1) " . " (cdar env2)))
                  (decrease-paths-below lit1)
                  (decrease-paths-below lit2)
                  (return (values lit2 (cdar env2) new-unif-prob 
                                  conn (cdr (path-to-subpath (member lit2 path))))))))))))

(defflag ms90-3-quick
  (flagtype boolean)
  (default nil)
  (subjects ms90-3 ms90-9 ms91-7 ms93-1 ms92-9 unification transmit)
  (mhelp "If T, do MS88 quick unification on dpairs in MS90-3.
If NIL, don't."))

(defun ho-banned-conn-p (conn)
  (declare (special *ms90-3-cgraph*))
  (let ((conn1 (literal-name (car conn)))
	(conn2 (literal-name (cdr conn)))
	(returnval nil))
    (if ms90-3-quick
	(progn (setq returnval
		     (or (member conn2 (cddr (assoc conn1 *ho-banned-conns-list*)) :test 'equal)
			 (member conn1 (cddr (assoc conn2 *ho-banned-conns-list*)) :test 'equal)))
	       (dolist (ho *ho-banned-conns-list*)
		       (dolist (ho2 (cdr ho))
			       (setf (gethash (cons ho2 (car ho)) *ms90-3-cgraph*) t)
			       (setf (gethash (cons (car ho) ho2) *ms90-3-cgraph*) t)))
	       (setq *ho-banned-conns-list* nil)
	       returnval)
      (or (member conn2 (cddr (assoc conn1 *ho-banned-conns-list*)) :test 'equal)
	  (member conn1 (cddr (assoc conn2 *ho-banned-conns-list*)) :test 'equal)))))

(defun quick-forbid (conn)
  (declare (special *ms90-3-cgraph*))
  (multiple-value-bind (value occursp)
		       (gethash conn *ms90-3-cgraph*)
		       (when occursp
			     (when (and value (memq mating-verbose '(med max)) (msg "Q")))
			     (return-from quick-forbid value)))
  (if (eq (quick-unification-main (list (connection-to-dpair-main conn)) nil nil) 'fail)
      (progn (setf (gethash conn *ms90-3-cgraph*) t) 
	     (setf (gethash (cons (cdr conn) (car conn)) *ms90-3-cgraph*) t)
	     t)
    (progn (setf (gethash conn *ms90-3-cgraph*) nil)
	   (setf (gethash (cons (cdr conn) (car conn)) *ms90-3-cgraph*) nil)
	   nil)))

(defun conn-unif-p (lit1 env1 lit2 env2 unif-prob &optional (start-time nil) (time-limit nil))
  (when (and *ho-banned-conns-list* (ho-banned-conn-p (cons lit2 lit1)))
	(when (memq mating-verbose '(med max)) (msg "B"))
	(return-from conn-unif-p nil))
  (when (and ms90-3-quick (quick-forbid (cons lit1 lit2)))
	(return-from conn-unif-p nil))
  (if first-order-mode-ms
      (multiple-value-bind (substs unifiablep)
	  (pfd-fo-unify-conn lit1 env1 lit2 env2)
	(if unifiablep
	    (progn
	      (unless unif-prob
		(setq unif-prob (make-unode :substs nil :dpairs nil
					    :fo-mode t)))
	      (multiple-value-bind (stack unifiablep)
		  (pfd-fo-unify nil substs (unode-substs unif-prob))
		(if unifiablep
		    (progn (setf (unode-substs unif-prob) stack)
			   (values unif-prob substs))
		    nil)))
	    nil))
      (let ((unode (make-unode :dpairs (list (initialize-conn
					      lit1 env1 lit2 env2)))))
	(when (ho-simpl unode)
	  (let ((dpairs (append (unode-substs unode) (unode-dpairs unode))))
	    (if unif-prob
		(if (unode-sons unif-prob)
		    (let ((new-sons nil))
		      (dolist (son (unode-sons unif-prob))
			(if new-sons
			    (progn
			      (setf (unode-dpairs son)
				    (append dpairs (unode-dpairs son)))
                          ;;;since new-sons is not empty, just add the
                          ;;;new dpairs to this son. They will get
                          ;;;evaluated at some future time.
                          ;;;Intentionally, delaying unification.
			      (push son new-sons))
			    (let ((new-son (copy-unode son)))
			      (setf (unode-dpairs new-son)
				    (append dpairs (unode-dpairs new-son)))
			      (when (ho-unify new-son (or max-search-depth most-positive-fixnum) start-time time-limit t)
				(setq new-sons (or (nreverse (unode-sons new-son))
						   (list new-son)))))))
		      (when new-sons
			(setf (unode-sons unif-prob) (nreverse new-sons))
			(values unif-prob dpairs)))
		    (let ((new-unif-prob (copy-unode unif-prob)))
		      (setf (unode-dpairs new-unif-prob)
			    (append dpairs (unode-dpairs new-unif-prob)))
		      (when (ho-unify new-unif-prob (or max-search-depth most-positive-fixnum) start-time time-limit t)
			(setq unif-prob new-unif-prob)
			(values unif-prob dpairs))))
		(when (ho-unify unode (or max-search-depth most-positive-fixnum) start-time time-limit t)
		  (values unode dpairs))))))))

;;;Returns NIL if P = (append (list literal) path) is not spanned by
;;;MATING. Otherwise returns the CONNECTION in MATING that spans P.

;;;This works because OPEN-PATH is a complete path. And hence, must
;;;contain ENV at the front.

;;this is never used! MB Fri Mar  7 15:53:47 1997

(defun same-lit-occ-p (lit index path env-stack)
  (dolist (elt path nil)
    (if (consp elt)
	;;check whether index is lower than index of top element in env-stack?
	(setq env-stack elt)
	(if (eq elt lit)
	    (when (= (cdar env-stack) index) (return t))))))

;;;Need to check only whether LIT occurs as the 2nd element in
;;;connection (1st . 2nd). If LIT were to be the first element, then
;;;we would have backed up at that point.

(defun order-contained-p (ord1 ord2)
   (dolist (elt ord1 T)
      (setq ord2 (member elt ord2))
      (if (null ord2) (return NIL))))
      
(defun lit-in-mating-p (lit path mating)
  (if (literal-p lit)
    (do ((mating mating (cddr mating)))
	((null mating) nil)
      (let ((conn (car mating)))
	(if (and (eq (cdr-conn conn) lit)
                 (order-contained-p (conn-subpath conn) path))
	    (return t))))))


