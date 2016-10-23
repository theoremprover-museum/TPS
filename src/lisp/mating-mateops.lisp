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

(context ms88)

(deffile mating-mateops
  (part-of ms88)
  (extension lisp)
  (mhelp "Interface to the mating search package."))

(defmateop init-mating
  (mate-alias start-new-mating)
  (mate-result-> ignore))

(defwffop init-mating
  (argnames)
  (argtypes)
  (applicable-q (lambda () t))
  (resulttype ignore)
  (mhelp "Initializes a new mating. This is the recommended way for
starting an interactive session in MS."))

(defun start-new-mating ()
  (setq active-mating (init-mating))
  (push active-mating (mating-list)))

(defmateop add-conn
  (mate-result-> ignore)
  (mate-alias add-conn))

(defmateop add-conn*
  (mate-result-> ignore)
  (mate-alias add-conn*))

(defmateop rem-conn*
  (mate-result-> ignore)
  (mate-alias rem-conn*))

(defvar ms-jform nil)

(defwffop add-conn*
  (argnames)
  (argtypes)
  (applicable-q (lambda () t))
  (resulttype ignore)
  (mhelp "Repeatedly call ADD-CONN."))

(defwffop rem-conn*
  (argnames)
  (argtypes)
  (applicable-q (lambda () t))
  (resulttype ignore)
  (mhelp "Repeatedly call REM-CONN."))

(defun add-conn* ()
  (eval (mate-command-interpreter  '(add-conn)))
  (msg t)
  (let (again) 
    (prompt-read again nil (msgf "Add another connection? ") 'yesno 'yes nil)
    (do () ((not again))
	(msg t)
	(eval (mate-command-interpreter '(add-conn)))
	(msg t t)
	(prompt-read again nil (msgf "Add another connection? ") 'yesno 'yes nil))))

(defun rem-conn* ()
  (locally (declare (special again)))
  (eval (ed-command-interpreter  '(rem-conn)))
  (msg t)
  (let (again) 
    (prompt-read again nil (msgf "Remove another connection? ") 'yesno 'yes nil)
    (do () ((not again))
	(msg t)
	(eval (mate-command-interpreter '(rem-conn)))
	(msg t t)
	(prompt-read again nil (msgf "Remove another connection? ") 'yesno 'yes nil))))

(defun complete-leaf-name (sym prefix)
  (cond ((symbolp sym) (symbol-name sym))
         ((integerp sym) 
          (concatenate 'string (symbol-name prefix) (princ-to-string sym)))
         (t sym)))

(defwffop add-conn  
  (argnames first second)
  (argtypes leaftype leaftype)
  (defaultfns (lambda (x y)
		(declare (special connection))
		(if (and (eq x '$) (eq y '$)
			 (boundp 'active-mating)
			 (boundp 'connection)
			 (numberp connection))
		    (let ((connection
			    (car (gethash connection (connections-array)))))
		      (list (literal-name (car connection))
			    (literal-name (cdr connection))))
		    (list x y))))
  (applicable-q
    (lambda (x y)
      (if (and (find-jform-name (complete-leaf-name x leaf-name) 
                                (or ms-jform (eproof-jform current-eproof)))
	       (find-jform-name (complete-leaf-name y leaf-name)
                                (or ms-jform (eproof-jform current-eproof))))
	  t (complain "Leaf node does not exist in JFORM. If you have altered"
" the expansion tree," t "use the command CJFORM to create a new JFORM."))))
  (resulttype ignore)
  (mhelp "Add a connection to the current mating. TPS will not allow you to
add a connection to a mating if adding it causes the resulting mating to be
non unifiable. No check is made to determine if the connection spans
an open path."))


(defun add-conn (sym1 sym2)
  (unless active-mating
    (start-new-mating)
    (setf (eproof-jform current-eproof)
	  (etree-to-jform (eproof-etree current-eproof))))
  (let ((lit1 (find-jform-name
	       (complete-leaf-name sym1 leaf-name)
	       (or ms-jform (eproof-jform current-eproof))))
	(lit2 (find-jform-name
	       (complete-leaf-name sym2 leaf-name)
	       (or ms-jform (eproof-jform current-eproof))))
	(mate-ffpair t))
    (let ((connection (find-potential-connection
		       lit1 (list lit2) (cgraph)
		       (mating-clist active-mating))))
      (if connection
	  (progn
	    (add-connection connection active-mating)
	    (if (if first-order-mode-ms
		    (multiple-value-bind (flag substs)
			(fo-compatible-p connection
					 (mating-utree active-mating))
		      (unless (eq flag 'fail)
			(setf (mating-utree active-mating) substs))
		      (if (eq flag 'fail) nil t))
		    (multiple-value-bind (flag root subst-hash-table
					       success-nodes)
			(unifiable-p active-mating t)
		      (setf (mating-utree active-mating) root)
		      (setf (mating-subst-hash-table active-mating)
			    subst-hash-table)
		      (setf (mating-success-nodes active-mating) success-nodes)
		      (when (eq flag 'fail)
			(modify-utree connection active-mating))
		      (if (eq flag 'fail) nil t)))
		(progn (msg T "Adding new connection: ")
		       (print-connection connection (connections-array)))
		(progn
		  (msg t "Adding this connection causes incompatibility." t
		       "Connection not added.")
		  (setf (incomp-clists)
			(store-incomp-clist (mating-clist active-mating)
					    (incomp-clists)))
		  (remove-last-connection active-mating))))
	  (complain "Can't add this connection: " sym1 " " sym2)))))

(defmateop rem-conn
  (mate-result-> ignore)
  (mate-alias rem-conn))

(defwffop rem-conn  
  (argnames first second)
  (argtypes leaftype leaftype)
  (defaultfns (lambda (x y)
		(declare (special connection))
		(if (and active-mating (eq x '$) (eq y '$)
			 (boundp 'connection) (numberp connection))
		    (let ((connection
			    (car (gethash connection (connections-array)))))
		      (list (literal-name (car connection))
			    (literal-name (cdr connection))))
		    (list x y))))
  (applicable-q
    (lambda (x y)
      (and active-mating
	   (find-jform-name (complete-leaf-name x leaf-name)
                            (or ms-jform (eproof-jform current-eproof)))
	   (find-jform-name (complete-leaf-name y leaf-name)
                            (or ms-jform (eproof-jform current-eproof))))))
  (resulttype ignore)
  (mhelp "Remove a connection from the current mating."))

(defun rem-conn (sym1 sym2)
 (let*  ((hxsym1 (intern-str (complete-leaf-name sym1 leaf-name)))
         (hxsym2 (intern-str (complete-leaf-name sym2 leaf-name)))
         (num-conn (gethash (cons hxsym1 hxsym2) (cgraph))))
    (when num-conn
      (if (integerp (mating-bktrack active-mating)) (incf (mating-bktrack active-mating)))
      (remove-connection num-conn active-mating)
      (modify-utree num-conn active-mating)
      (setf (mating-subst-hash-table active-mating) nil)
      (setf (mating-first-path active-mating) t)
      (msg t "Removed connection: ")
      (print-connection num-conn (connections-array)))))

(defmateop rem-last-conn
  (mate-result-> ignore)
  (mate-alias rem-last-conn))


(defwffop rem-last-conn  
  (argnames)
  (argtypes)
  (applicable-q (lambda ()  (if active-mating t
			      (complain "No mating in progress."))))
  (resulttype ignore)
  (mhelp "Remove the last connection to the current mating."))

(defun rem-last-conn ()
  (when (mating-clist active-mating)
    (if (integerp (mating-bktrack active-mating)) (incf (mating-bktrack active-mating)))
    (setf (mating-first-path active-mating) t)
    (msg t "Removed connection: ")
    (let ((result (remove-last-connection active-mating)))
      (print-connection result (connections-array))
      (modify-utree result active-mating))))

(defmateop complete-p
  (mate-result-> ignore)
  (mate-alias complete-p))

(defwffop complete-p
  (argnames)
  (argtypes)
  (applicable-q (lambda ()  (if active-mating t
			      (complain "No mating in progress."))))
  (resulttype ignore)
  (mhelp "Test whether current mating is complete. Will return a path that
is not spanned by the mating otherwise."))

(defun complete-p ()
  (if (mating-completep active-mating)
      (msg t "Mating is complete.")
      (let ((next-open-path
	      (find-next-openpath
		(or ms-jform (etree-to-jform (eproof-etree current-eproof)))
		nil (cgraph) (mating-clist active-mating))))
	(unless next-open-path 
	  (setf (mating-completep active-mating) t))
	(if next-open-path
	    (msg t "Mating is not complete as the following path is open:" t
		 (l (mapcar 'show-prop-name next-open-path)))
	    (msg t "Mating is complete.")))))

(defmateop show-mating
  (mate-alias show-mating)
  (mate-result-> ignore))

(defwffop show-mating
  (argnames)
  (argtypes)
  (applicable-q (lambda ()  (if active-mating t
			      (complain "No mating in progress.")))) 
  (mhelp "Show the connections in the current mating."))

(defun show-mating ()
  (msg t "Active mating: ")
  (if (consp (car (mating-clist active-mating)))
      (msg t (l (mating-clist active-mating)))
      (print-clist (mating-clist active-mating) (connections-array)))
  (if (mating-completep active-mating)
      (msg t "is complete.")))

(defmateop apply-substs
  (mate-alias apply-substs-ms)
  (mate-result-> ignore))

(defwffop apply-substs-ms
  (argnames)
  (argtypes)
  (applicable-q (lambda () (and active-mating
				 (mating-completep active-mating)))) 
  (mhelp "Apply substitutions found during mating search to JFORM. Applicable
only if mating is complete."))

(defun apply-substs-ms ()
  (unless (eproof-jform current-eproof)
    (setf (eproof-jform current-eproof)
	  (etree-to-jform (eproof-etree current-eproof))))
  (apply-substs-to-jform
    (eproof-jform current-eproof)
    (find-substs-for-mating active-mating)))

(defmateop show-substs
  (mate-alias show-substs)
  (mate-result-> ignore))

(defwffop show-substs
  (argnames)
  (argtypes)
  (applicable-q (lambda ()  (and active-mating
				 (mating-completep active-mating))))
  (mhelp "Show the substitutions suggested by mating search for the complete
    active mating."))

(defun show-substs ()
  (if (mating-success-nodes active-mating)
      (print-subst-stack
	(node-subst-stack (car (mating-success-nodes active-mating))))
      (print-subst-stack (mating-utree active-mating))))

(context stats)

(defmateop stats
  (mate-result-> ignore)
  (mate-alias show-mating-stats))

(defwffop show-mating-stats
  (argnames)
  (argtypes)
  (applicable-q (lambda ()
		  (and (boundp 'current-eproof) (boundp 'active-mating)
		       active-mating)))
  (mhelp "Display statistics for the active mating and totals for all
matings in this expansion proof."))


(defun show-mating-stats ()
  (when (and (boundp 'active-mating) active-mating)
    (msgf "Statistics for active mating:")
    (print-stats (mating-stats active-mating) *standard-output* 0))
  (when (and (boundp 'current-eproof) current-eproof)
    (msg -2 "Totals for all matings:")
    (print-stats (eproof-stats current-eproof) *standard-output* 0)
    (msg t "Number of vertical paths: "
         (number-of-vertical-paths (or ms-jform (eproof-jform current-eproof)))
         t "Number of leaves: "
         (length (eproof-leaf-list current-eproof))))
   (msg t))

(defmateop del-dup-conns
  (mate-result-> ignore)
  (mate-alias delete-duplicate-conns))

(defwffop delete-duplicate-conns
  (argnames)
  (argtypes)
  (applicable-q (lambda ()
		  (and (boundp 'current-eproof) (boundp 'active-mating))))
  (mhelp "Deletes duplicate connections from a mating. This should be necessary
    only for propositional formulas."))

(defun delete-duplicate-conns ()
  (let* ((clist (mating-clist active-mating))
	 (length (length clist)))
    (setf (mating-clist active-mating)
	  (if (consp (car clist)) (delete-duplicates clist :test #'equal)
	      (delete-duplicates clist)))
    (decf length (length (mating-clist active-mating)))
    (if (zerop length)
	(msg t "No connections removed.")
	(msg t length " connections removed."))))

(defmexpr daterec
  (argnames name type comment)
  (argtypes symbol lib-argtype string)
  (defaultfns (lambda (z y w)
		(list (if (eq z '$) dproof z)
		      (if (eq y '$) 'gwff y)
		      (if (eq w '$) "" w))))
  (arghelp "Name of theorem" "Type of library object" "Your choice")
  (mainfns daterec)
  (mhelp "Records times used in the following processes:
DIY, Mating Search, Merging Expansion Tree, Proof Transformation.
All times recorded are in seconds.
Internal-runtime includes GC-time.
GC-time is garbage-collecting-time.
I-GC-time is Internal-runtime minus GC-time.
DATEREC also records the values of the flags listed in RECORDFLAGS,
and will offer the user the chance to reset the provability 
status of a gwff in the library."))


;;;The following command is used to show time used
;;;in several processes.

(defmexpr display-time
  (argnames name)
  (argtypes symbol)
  (defaultfns (lambda (z)
		(list (if (eq z '$) 'all z))))
  (arghelp "Name of process")
  (mainfns display-time)
  (mhelp "Show time used in several processes:
display-time diy: show the time used in DIY process
display-time mating: show the time used in mating-search process
display-time merge: show the time used in merging-expansion-tree process
display-time eproof: show the time used in proof-transformation process
display-time all: show all the times above
All times are in seconds.
Internal-runtime includes GC-time.
GC-time is garbage-collecting-time.
I-GC-time is Internal-runtime minus GC-time."))

