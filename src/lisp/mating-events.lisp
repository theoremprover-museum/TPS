;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)
;  (part-of mating-search)
(part-of ms88)

(context ms88)

(deffile mating-events
  (part-of ms88)
  (extension lisp)
  (mhelp "Contains functions used in signalling events during mating search."))


(defevent added-conn
  (event-args conn)
  (write-when never)
  (signal-hook signal-added-conn)
  (mhelp "Event which is signalled whenever a connection is added to a mating."))

(defun signal-added-conn (conn)
  (incf (stats-num-added (mating-stats active-mating)))
  (incf (stats-num-added (eproof-stats current-eproof)))
  (case mating-verbose
    (max (msgf "Adding connection: ")
	 (print-connection conn (connections-array)))
    (med (msg "+" conn))
    (min (msg "+")))
  (when rec-ms-file
    (let ((reroute-close-message nil))
      (reroute-output-append rec-ms-filename *default-pathname-defaults*
	(msgf "Adding connection: ")
	(print-connection conn (connections-array))))))

(defevent removed-conn
  (event-args conn)
  (write-when never)
  (signal-hook signal-removed-conn)
  (mhelp "Event which is signalled whenever a connection is removed from a mating."))

(defun signal-removed-conn (conn)
  (incf (stats-num-removed (mating-stats active-mating)))
  (incf (stats-num-removed (eproof-stats current-eproof)))
  (case mating-verbose
    (max (msgf "Removing connection: ") 
	 (print-connection conn (connections-array)))
    (med (msg "-" conn))
    (min (msg "-")))
  (when rec-ms-file
    (let ((reroute-close-message nil))
      (reroute-output-append rec-ms-filename *default-pathname-defaults*
	(msgf "Removing connection: ")
	(print-connection conn (connections-array))))))

(defevent dupe
  (event-args)
  (write-when never)
  (signal-hook signal-dupe)
  (mhelp "Event which is signalled whenever a variable duplication is
done in a mating."))

(defun signal-dupe ()
  (when (and (boundp 'current-eproof)(mating-p active-mating))
    (incf (stats-num-dupes (mating-stats active-mating)))
    (incf (stats-num-dupes (eproof-stats current-eproof)))
    (case mating-verbose
      (max (if interrupt-enable
	       (msgf "Duplicating. Strategy: " duplication-strategy)))
      (t (msg "2")))
    (when rec-ms-file
      (let ((reroute-close-message nil))
	(reroute-output-append rec-ms-filename *default-pathname-defaults*
	  (msgf "Duplicating. Strategy: " duplication-strategy))))))

(defevent considered-conn
  (event-args conn)
  (write-when never)
  (signal-hook signal-considered-conn)
  (mhelp "Event which is signalled whenever a connection is considered."))

(defun signal-considered-conn (conn)
  (declare (ignore conn))
  (when active-mating
    (incf (stats-num-conns-considered (mating-stats active-mating))))
  (incf (stats-num-conns-considered (eproof-stats current-eproof)))
  (case mating-verbose
    ((max med) (msg "*"))))

(defevent dupe-var
  (event-args var)
  (write-when never)
  (signal-hook signal-dupe-var)
  (mhelp "Event which is signalled whenever a variable is duplicated."))

(defun signal-dupe-var (var)
  (when (and (boundp 'current-eproof)(mating-p 'active-mating))
    (incf (stats-num-vars-duped (mating-stats active-mating)))
    (incf (stats-num-vars-duped (eproof-stats current-eproof)))
    (case mating-verbose
      (max (msgf "Duplicating var:" (var . gwff))))
    (let ((style 'generic))
      (when rec-ms-file
	(let ((reroute-close-message nil))
	  (reroute-output-append rec-ms-filename *default-pathname-defaults*
	    (msgf "Duplicating var: " (var . gwff))))))))

(defevent primsub
  (event-args var sub)
  (write-when never)
  (signal-hook signal-primsub)
  (mhelp "Event which is signalled whenever a primitive substitution
is applied to an expansion tree."))

(defun signal-primsub (var sub)
  (incf (stats-num-primsubs (mating-stats active-mating)))
  (incf (stats-num-primsubs (eproof-stats current-eproof)))
  (case mating-verbose
    (max (msgf "Applying primitive substitution for " (var . gwff) ": ")
	 (prt-prim sub))
    (med (msgf "Applying primitive substitution."))
    (min (msg "P")))
  (let ((style 'generic))
    (when rec-ms-file
      (let ((reroute-close-message nil))
	(reroute-output-append rec-ms-filename *default-pathname-defaults*
	  (msgf "Applying primitive substitution for " (var . gwff) ": ")
	  (prt-prim sub))))))

(defevent mate-subsumed-test
  (event-args)
  (write-when never)
  (signal-hook signal-mate-subsumed-test)
  (mhelp "Event which is signalled whenever a mating is tested for 
subsumption."))

(defun signal-mate-subsumed-test ()
  (incf (stats-mate-subsume-tests (mating-stats active-mating)))
  (incf (stats-mate-subsume-tests (eproof-stats current-eproof)))
  (case mating-verbose
    (max (msgf "Testing mating for subsumption."))
    (med (msg " MST ")))
  (when rec-ms-file
    (let ((reroute-close-message nil))
      (reroute-output-append rec-ms-filename *default-pathname-defaults*
	(msgf "Testing mating for subsumption.")))))

(defevent mate-subsumed-true
  (event-args)
  (write-when never)
  (signal-hook signal-mate-subsumed-true)
  (mhelp "Event which is signalled whenever a mating is subsumed by
an incompatible mating."))

(defun signal-mate-subsumed-true ()
  (incf (stats-mate-subsume-true (mating-stats active-mating)))
  (incf (stats-mate-subsume-true (eproof-stats current-eproof)))  
  (case mating-verbose
    (max (msgf "Mating subsumed by incompatible/inextensible mating."))
    (med (msg " MSS ")))
  (when rec-ms-file
    (let ((reroute-close-message nil))
      (reroute-output-append rec-ms-filename *default-pathname-defaults*
	(msgf "Mating subsumed by incompatible/inextensible mating.")))))

(defevent unif-subsumed-test
  (event-args)
  (write-when never)
  (signal-hook signal-unif-subsumed-test)
  (mhelp "Event which is signalled whenever a set of disagreement pairs
unification is tested for subsumption."))

(defun signal-unif-subsumed-test ()
  (incf (stats-unif-subsume-tests (mating-stats active-mating)))
  (incf (stats-unif-subsume-tests (eproof-stats current-eproof)))
  (case mating-verbose
    (max (msgf "Testing disagreement pairs for subsumption."))
    (med (msg " UST ")))
  (when rec-ms-file
    (let ((reroute-close-message nil))
      (reroute-output-append rec-ms-filename *default-pathname-defaults*
	(msgf "Testing disagreement pairs for subsumption.")))))

(defevent unif-subsumed-true
  (event-args)
  (write-when never)
  (signal-hook signal-unif-subsumed-true)
  (mhelp "Event which is signalled whenever a set of disagreement pairs is
found to be subsumed by an ununifiable set."))

(defun signal-unif-subsumed-true ()
  (incf (stats-unif-subsume-true (mating-stats active-mating)))
  (incf (stats-unif-subsume-true (eproof-stats current-eproof)))
  (case mating-verbose
    (max (msgf "Disagreement pairs subsumed."))
    (med (msg " USS ")))
  (when rec-ms-file
    (let ((reroute-close-message nil))
      (reroute-output-append rec-ms-filename *default-pathname-defaults*
	(msgf "Disagreement pairs subsumed.")))))

(defevent start-time
  (event-args)
  (write-when never)
  (signal-hook signal-start-time)
  (mhelp "Event which is signalled whenever a mating should have its run
time started, such as when it becomes the active mating."))

(defun signal-start-time ()
  (when active-mating
    (setf (cdr (stats-time-used (mating-stats active-mating)))
	  (tps-get-internal-run-time))))

(defevent stop-time
  (event-args)
  (write-when never)
  (signal-hook signal-stop-time)
  (mhelp "Event which is signalled whenever a mating should have its run
time stopped, such as when it is no longer the active mating."))

(defun signal-stop-time ()
  (when active-mating
    (let ((time-used (- (tps-get-internal-run-time)
			(cdr (stats-time-used
			       (mating-stats active-mating))))))
      (incf (car (stats-time-used (mating-stats active-mating))) time-used)
      (incf (car (stats-time-used (eproof-stats current-eproof))) time-used))))

(defevent mating-changed
  (event-args)
  (write-when never)
  (signal-hook signal-mating-changed)
  (mhelp "Event which is signalled whenever a different mating is considered."
	 ))

(defun signal-mating-changed ()
  (when active-mating
    (case mating-verbose
        ((min med)
	 (msg (mating-clist active-mating)))
	(max 
          (progn
	    (msg t "Changing active mating:")
	    (print-clist (mating-clist active-mating) (connections-array)))))
    (when rec-ms-file
      (let ((reroute-close-message nil))
	(reroute-output-append rec-ms-filename *default-pathname-defaults*
	  (msgf "Changing active mating:" t)
	  (print-clist (mating-clist active-mating) (connections-array)))))))

(defevent incomp-mating
  (event-args)
  (write-when never)
  (signal-hook signal-incomp-mating)
  (mhelp "Event which is signalled whenever an incompatible mating is found."))

(defun signal-incomp-mating ()
  (when active-mating
    (case mating-verbose
        (min (msg "%"))
	((max med)
          (progn (msg T "Mating ")
	       (print-clist (mating-clist active-mating)
			    (connections-array))
	       (msg "  is not unifiable. Backtracking."))))
    (when rec-ms-file
      (let ((reroute-close-message nil))
	(reroute-output-append rec-ms-filename *default-pathname-defaults*
	  (msg T "Mating not unifiable: ")
	  (print-clist (mating-clist active-mating) (connections-array)))))))
