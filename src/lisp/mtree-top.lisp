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

(deffile mtree-top
  (part-of mst)
  (extension lsp)
  (mhelp "Defines matingstree toplevel."))

(context subtoplevels)

(deftoplevel mtree-top
  (top-prompt-fn mtree-top-prompt)
  (command-interpreter mtree-command-interpreter)
  (print-* mtree-printer-*)
  (top-level-category mtreeop)
  (top-level-ctree mtree-command-ctree)
  (top-cmd-decode mate-opdecode)
  (mhelp "The top level of MTREE."))

;;;
;;;The category mtreeop is realy the category of mating tree commands.
;;;......
;;;

(eval-when (compile load eval)
(defcategory mtreeop
  (define defmtreeop)
  (properties
    (mtree-alias single)
    (mtree-move single)
    (mtree-print single)
    (mtree-default single)
    (mtree-args single)
    (mhelp single))
  (global-list global-mtreeoplist)
  (mhelp-line "matingstree command")
  (mhelp-fn core::mtreeop-mhelp)
  (scribe-one-fn 
   (lambda (item)
      (maint::scribe-doc-command 
       (format nil "@IndexOther(~A)" (symbol-name item))
       (get (get item 'mtree-alias) 'argnames)
       (or (cdr (assoc 'mtreeop (get item 'mhelp)))
	   (cdr (assoc 'wffop 
		       (get (get item 'mtree-alias)
			    'mhelp)))))))))

(defvar mtree-level 0)

(defvar mtree-command-ctree nil)

(defun initialize-mtree-ctree ()
  (initialize-top-level-ctree 'mtree-top))

(defun mtree-top-prompt (id)
  (declare (special mtree-level))
  (let ((x (cond ((= mtree-level 1) "") (T mtree-level))))
       (format nil "<~:[~A:~;~A~]Mtree:~A>"
                   (= mtree-level 1) x id)))

(defun mtree-print-* (result) (declare (ignore result)))

(defvar current-mating nil)

(defvar current-matingstree nil)

(defvar last-etree nil)

(defvar *matingstree-temp* nil)

(defun mtree-command-interpreter (cmd)
  (declare (special nodestack cmdstack current-mating current-matingstree))
  (do ((cmdlist nil (cons nextcmd cmdlist))
       (cmd cmd rest)
       (command (car cmd) (car rest))
       (rest (cdr cmd) (cdr rest))
       (nextcmd nil))
      ((null cmd)
        `(progn ,@(nreverse cmdlist)))
      (declare (special rest))
      (setq nextcmd
            (cond ((integerp command) (mtree-integerp-interpreter command))
                  ((consp command) (consp-interpreter command))
                  ((and (symbolp command) (get command 'mtreeop))
                   (mtree-op-interpreter command))
                  ((and (symbolp command)
		      (or (get command 'mateop) (get command 'wffop)))
                   (mateop-interpreter command))
                  ((and (symbolp command) 
                        (or (get command 'mexpr) (get command 'reviewcmd)))
                   (mexpr-interpreter command))
                  ((and (symbolp command) (get command 'flag))
                   (flag-interpreter command))
                  (t (misc-interpreter command))))))


(defun merge-args (arglist rest)
 (if arglist 
     (if rest (cons (car rest) (merge-args (cdr arglist) (cdr rest))) arglist)
     rest))
          
(defun mtree-op-interpreter (command)
  (declare (special rest))
  (let ((alias (get command 'mtree-alias))
;        (result (get command 'mtree-result)))
	)
     (cond ((get command 'mtree-move)
            (setq rest 
              (merge-args (append (get command 'mtree-default) 
                                  (list current-matingstree)) 
                           rest)))
           ((get command 'mtree-default)
            (setq rest (merge-args (get command 'mtree-default) rest))))
     (let* ((no-args (or (get command 'mtree-args) 0))
	    (more-p (> (length rest) no-args))
	    (new-rest (if more-p (nthcdr no-args rest) nil))
	    (args (if more-p (ldiff rest new-rest) rest)))
       (setq rest new-rest)
      `(%catch% (progn
                    (if ,(get  command  'mtree-move)
                        (progn (setq *matingstree-temp* (mtreeopdecode ',(cons alias args)))
                     	      (push current-matingstree nodestack)
                              (setq current-matingstree *matingstree-temp*))
                        (mtreeopdecode ',(cons alias args)))
                    (if ,(get  command  'mtree-print) (print current-matingstree)))
                (fail (complain core::expand-catch-throw t)
		      (complain "Operation " ',command " not performed."))))))

(defun mtree-integerp-interpreter (command)
  (declare (special rest) (ignore command)))
;added ignore here

(defun mtreeopdecode (command)
  (declare (special strong-defaults))
  (let ((keyword (car command))
	(alias (get (car command) 'mtree-alias))
	 appfn mainfn result)
    (multiple-value-bind
     (internal-arglist external-arglist)
     (prompt-values keyword
		    (copy (cdr command))
		    (getkey keyword alias 'argtypes)
		    (or (getkey keyword alias 'wffargtypes)
			(mapcar #'(lambda (x) (declare (ignore x)) nil)
				(getkey keyword alias 'argtypes)))
		    (getkey keyword alias 'wffop-typelist)
		    (getkey keyword alias 'defaultfns)
		    (funcall strong-defaults keyword alias)
		    (get-mate-argnames keyword)
		    (getkey keyword alias 'arghelp))
     (declare (ignore external-arglist))
    (setq appfn (or (get keyword 'mtree-applicable-p)
		    (getkey keyword alias 'applicable-p)
		    (getkey keyword alias 'applicable-q)))
    (if (and appfn (not (apply appfn internal-arglist)))
	(throwfail keyword " not applicable."))
    (setq mainfn (cond (alias) (t keyword)))
    (%catch% (setq result (apply mainfn internal-arglist))
	     (fail (complain f "Error from " mainfn ".  " 
                             core::expand-catch-throw)
		   (throwfail "Operation aborted.")))
    result)))

(defmexpr mtree 
  (argtypes gwff0-or-eproof yesno yesno yesno)
  (argnames gwff deepen reset window)
  (arghelp "Gwff or Eproof" "Deepen?" "Reset counters?" "Open Vpform Window?")
  (defaultfns (lambda (z w r s)
		(list (if (eq z '$) 
			  (cond ((eproof-p current-eproof) current-eproof)
				((eproof-p last-eproof) last-eproof)
				(t '$))
			  z) 
		      (if (eq w '$) t w) (if (eq r '$) t r) (if (eq s '$) nil s))))
  (mainfns mtree-mating)
  (mhelp "Begin to enter the mating tree top level."))


(defun mtreetop ()
 (let ((top-prompt-fn #'mtree-top-prompt)
       (mtree-level (1+  mtree-level))
       (command-interpreter #'mtree-command-interpreter)
       (top-level 'mtree-top)
       (print-* #'mtree-print-*)
       (strong-defaults 'strong-mate-defaults)
       )
  (declare (special strong-defaults))
  (secondary-top)))

(defvar *initial-lits* nil)

(defvar *mtree-options* nil)

(defun mtree-mating (gwff deepen reset window)
  (%catch% (progn (when reset (expunge-vars))
		  (mate-wff-prefix gwff deepen window)
		  (setq *initial-lits* (jform-to-literal-list (auto::eproof-jform current-eproof) nil))
		  (setq already-added-clists nil)
		  (setq *mtree-options* (list gwff deepen reset window)) 
                  (or (and current-matingstree (eproof-p gwff)) 
                      (progn 
                        (when reset
                              (setf (get matingstree-name 'core::counter) 0)
                              (setf (get obligation-name 'core::counter) 0))
                        (init-matingstree nil)))
                  (mtreetop) 
                  (throwfail "Mating search tree top level aborted."))
  (exit-inferior-top "Good bye!")))

(defmtreeop leave
   (mtree-alias exit-mtree)
   (mhelp "leaving the mtree top level."))

(defun  exit-mtree-prior ()
 (when current-matingstree 
       (setq active-mating (matingstree-mating current-matingstree))
       (when (and (not (matingstree-merged current-matingstree))
                  (boundp 'current-eproof)
	          (eproof-p current-eproof)
	          (etree-p (eproof-etree current-eproof))
	          (boundp 'active-mating)
	          (mating-p active-mating)
	          (mating-completep active-mating)
                  (query "Merge the expansion tree?" t))
            (msgf "Merging the expansion tree.  Please stand by." t)
            (merge-tree)
            (setf (matingstree-merged current-matingstree) T)))
   (%throw% '|[Normal Exit of Matings Tree Top Level.]| exit-inferior-top))

(defun  exit-mtree ()
 (when current-matingstree
       (when (get-open-obs (matingstree-obligation current-matingstree))
	   ;then we aren't at a complete node
	   (let* ((ll (get-live-leaves-main 
		      (mst-goto 'mstroot current-matingstree)))
		  (llc (car (remove-if #'(lambda (x) (get-open-obs (matingstree-obligation x))) ll))))
	     (when (and llc
			(query (format nil "Move to complete mtree node ~A?" (princ-to-string (matingstree-name llc))) t))
		   (eval (mtree-command-interpreter (list 'goto (matingstree-name llc)))))))
       (setq active-mating (matingstree-mating current-matingstree))
       (when (and (mst-quiet-complete-p)
		  (not (matingstree-merged current-matingstree))
                  (boundp 'current-eproof)
	          (eproof-p current-eproof)
	          (etree-p (eproof-etree current-eproof))
	          (boundp 'active-mating)
	          (mating-p active-mating)
	          (mating-completep active-mating)
		  (query "Choose branch?" t))
	    (mst-choose-branch)))
 (exit-mtree-prior))

;;;-------------------------------------------------------------

(defun matingstree-q (wff)
  (matingstree-p wff))

(defun copy-utree (utree &optional (first-order nil))
  (if first-order 
      (copy utree)
      (let ((hxsons (node-sons utree))
            (hxdupnode (copy-node utree)))
	(when (node-free-vars hxdupnode) (setf (node-free-vars hxdupnode) (free-vars-in-etree current-eproof)))
        (if hxsons (setf (node-sons hxdupnode); utree)
			 (mapcar #'copy-utree hxsons)))
        hxdupnode)))

(defun hxcopy-mating (mating)
  (and mating
    (let ((hxutree (mating-utree mating))
         (hxdupmating (copy-mating mating)))
       (if hxutree 
          (setf (mating-utree hxdupmating) 
                (copy-utree hxutree first-order-mode-ms)))
        hxdupmating)))

(defflag mt-dups-per-quant
  (flagtype integer+-or-infinity)
  (default :infinite)
  (subjects mtree-top etrees transmit)
  (mhelp "The maximum number of times that each individual quantifier
may be duplicated in the MATINGSTREE search procedures. This flag is 
overridden by NUM-OF-DUPS, which governs the maximum total number of 
duplications of all quantifiers in the matingstree search."))

(defun extend-leaf (leaf tree)
  (label-obtree leaf tree)
  (setf (matingstree-parent leaf) tree)
;  (if (check-num-of-dups (obligation-used-univs (matingstree-obligation leaf)))
;      (progn (if (memq mating-verbose '(med max))
;		 (msgf "Killing new mtree leaf: too many duplications" t))
;	     (setf (matingstree-dead leaf) t)))
; commented out until it works properly. MB Tue Feb 28 10:47:28 1995
  (setf (matingstree-sons tree)
        (append (matingstree-sons tree) (list leaf))))

(defun label-obtree (leaf tree)
  (let* ((old-obs (get-all-obs (matingstree-obligation tree)))
	 (new-obs (get-all-obs (matingstree-obligation leaf)))
	 (diff (set-difference new-obs old-obs :test #'(lambda (x y) (eq (obligation-name x) (obligation-name y)))))
	 (name (matingstree-name leaf)))
    (dolist (ob diff)
	    (setf (obligation-from-expanding ob) name))
))

(defun get-all-obs (&optional (obligation current-obligation))
  (let ((rl (list obligation)))
    (do ((plist (obligation-next obligation) (rec-next plist))
	 (old-plist nil plist))
	((equal plist old-plist) rl)
	(if old-plist (setq rl (append (rec-find-all old-plist) rl))))))

(defun rec-find-all (plist)
  (let ((returned nil))
    (dolist (ob (remove-if #'null (flatten-mstlist plist)))
	    (setq returned (cons ob returned)))
    returned))

(context mtree-ops)

(defmtreeop init
   (mtree-alias init-matingstree)
   (mtree-print T)
   (mhelp "Initialize the matingstree. This is done automatically 
when you enter the matingstree top level, but can be used 
subsequently to return everything to the state it was in when 
you first entered the mtree top level."))

(defun init-matingstree (&optional (manual t))
   (if (not current-eproof) 
       (throwfail "The current-eproof is empty. You can use SUB command
to assign an expansion proof to current-eproof."))
   (if (and current-matingstree manual)
       (let ((gwff (car *mtree-options*))
	     (deepen (cadr *mtree-options*))
	     (reset (caddr *mtree-options*)))
	 (mate-wff-prefix gwff deepen nil)
	 (setq *initial-lits* (jform-to-literal-list (auto::eproof-jform current-eproof) nil))
	 (setq already-added-clists nil)
	 (or (and current-matingstree (eproof-p gwff)) 
	     (progn 
	       (when reset
		     (setf (get matingstree-name 'core::counter) 0)
		     (setf (get obligation-name 'core::counter) 0))))))
   (setf (eproof-jform current-eproof)
	 (etree-to-jform (eproof-etree current-eproof)))
   (setq current-obligation 
         (make-obligation :jform (eproof-jform current-eproof)
                          :used-univs (init-used-univs (eproof-jform current-eproof))))
   (auto-expand current-obligation)
   (setq already-added-clists nil)
   (setq current-matingstree (make-matingstree :mating (init-mating)
                                               :obligation current-obligation)))

;;;------------------------------------------------------------------

(defmtreeop add-conn
   (mtree-alias add-conn-ob)
   (mtree-args 4)
   (mtree-print T)
   (mhelp "Add a connection. The subsumption is considered.
The usage of the command is exactly as the usage of ADD-CONN in MATE."))

(defwffop add-conn-ob
;  (argnames first second third fourth)
  (argnames literal1 oblig1 literal2 oblig2)
  (argtypes leaftype symbol-or-integer leaftype symbol-or-integer)
  (defaultfns (lambda (x y u v)
                 (let* ((x1 (if (eq x '$) x (intern (complete-leaf-name x leaf-name))))
                        (y1 (if (eq y '$)
                                (and (neq '$ x) 
                                   (or (car (occurs-in-obtree x1 current-obligation)) 
                                        (throwfail "There is no leaf " x1 " in unfulfiled obligations.")))
                                (let ((y2 (if (obligation-p y) y 
                                              (obt-goto y current-obligation))))
                                   (if (obligation-next y2)
                                       (throwfail "Node " y2 " is not a leaf in the obligation tree.") y2))))
                        (u1 (if (eq u '$) u (intern (complete-leaf-name u leaf-name))))
                        (v1 (if (eq v '$)
                                (and (neq '$ y) (neq '$ u) 
                                     (or (occurs-in-obtree-path u1 y1) 
                                         (throwfail "Sorry, leaf " u1 " is not available to mate with leaf " x1 ".")))
                                (if (obligation-p v) v
                                    (obt-goto v current-obligation)))))
                    (list x1 y1 u1 v1))))
  (resulttype ignore)
  (mhelp "Add a connection to the current mating. TPS will not allow you to
add a connection to a mating if adding it causes the resulting mating to be
non unifiable. No check is made to determine if the connection spans
an open path."))

;;;----------------------------------------------------------------

(defmtreeop rem-node
   (mtree-alias mst-rem-conn)
   (mtree-args 0)
   (mtree-print T)
   (mhelp "Remove the last connection. The subsumption is considered.
If the node is the root, the whole matingstree is removed. The 
usage of the command is exactly the as the usage of REM-LAST-CONN. 
Please check the help message for REM-LAST-CONN if necessary."))

(defun mst-rem-conn () 
   (let ((hxparent (matingstree-parent current-matingstree)))
     (if hxparent
         (let ((hxsiblings (matingstree-sons hxparent)))
              (setq hxsiblings 
                (delete current-matingstree hxsiblings 
                   :test #'(lambda (x y) (eq (matingstree-name x) (matingstree-name y)))))
              (setf (matingstree-sons hxparent) hxsiblings)
              (setq current-matingstree hxparent)
              (setq current-obligation (matingstree-obligation hxparent)))
         (progn (setq current-matingstree nil)
                (setq current-obligation nil)))))

(defmtreeop prune
   (mtree-alias mst-prune)
   (mtree-args 0)
   (mtree-print T)
   (mhelp "Remove all dead leaves below (but not including) 
the current matingstree."))

(defun mst-prune (&optional (mst current-matingstree))
  (let ((sons (matingstree-sons mst)))
    (setf (matingstree-sons mst) (delete-if #'(lambda (x) (eq (matingstree-dead x) t)) sons))
    (mapc #'mst-prune (matingstree-sons mst))))

(defmtreeop choose-branch
   (mtree-alias mst-choose-branch)
   (mtree-args 0)
   (mtree-print T)
   (mhelp "Remove all matingstree branches except the one leading to the 
current matingstree node (which must be a leaf of the matingstree, and must
be complete). This will also do some preliminary merging, by deleting all 
of the jforms which are associated with the deleted nodes."))

(defun mst-choose-branch (&optional (mst current-matingstree))
  (when (matingstree-sons mst)
	(throwfail t "The current matingstree node is not a leaf." t))
  (let ((active-mating (matingstree-mating mst)))
    (unless (mating-completep active-mating)
	    (mst-complete-p)
	    (unless (mating-completep (matingstree-mating mst))
		    (throwfail t "The current matingstree node is not complete" t))))
  (let ((preservable-leaves (append nil;(mapcar 'show-prop-name *initial-lits*)
				    (find-lits-in-mating (matingstree-mating mst))))
	(exp-nodes (find-etree-nodes 'expansion-p (eproof-etree current-eproof))))
    (dolist (exp exp-nodes)
	    (let ((t-and-f-string (mapcar #'(lambda (x) 
					      (useless-expansion-p x preservable-leaves)) 
					  (etree-components exp))))
	      (setf (etree-components exp) (remove-if 'null (mapcar  #'(lambda (x y) (if x y nil)) t-and-f-string (etree-components exp))))
	      (setf (expansion-terms exp)  (remove-if 'null (mapcar  #'(lambda (x y) (if x y nil)) t-and-f-string (expansion-terms exp))))
	      (setf (expansion-prim-vars exp) (remove-if 'null (mapcar  #'(lambda (x y) (if x y nil)) t-and-f-string (expansion-prim-vars exp))))))
    ;;(yuck! MB)
    (update-statuses (eproof-etree current-eproof)))
  (do ((node (matingstree-parent mst) (matingstree-parent node))
       (old-node mst node))
      ((not node))
      (setf (matingstree-sons node) (list old-node))))

(defun useless-expansion-p (exp lits)
;  returns t if some of the literals should be kept, nil o/w
    (if
	(intersection (mapcar 'princ-to-string lits) 
		      (mapcar 'princ-to-string (find-etree-nodes 'leaf-p exp)) :test 'string=)
	t
      nil))
    
(defun find-lits-in-mating (mating)
  (let ((mating (mating-clist mating)))
    (setq mating (mapcar #'(lambda (x) (car (gethash x (connections-array)))) mating))
    (find-lits-in-mating-real mating)))

(defun find-lits-in-mating-real (mating)
  (if (null mating) nil
    (append (list (show-prop-name (caar mating)) (show-prop-name (cdar mating)))
	    (find-lits-in-mating-real (cdr mating)))))

(defmtreeop complete-p
   (mtree-alias mst-complete-p)
   (mtree-args 0)
   (mhelp "Check the completeness of the current mating. The usage of the command is 
exactly the same as the usage of the mate command COMPLETE-P."))

(defun unfulfiled-ob (obligation)
  (and (not (obligation-closed obligation))
       (if (null (obligation-next obligation))
           (if (obligation-closed obligation) nil obligation)
           (let (result)
            (dolist (subob (obligation-next obligation))
                (setq result (unfulfiled-ob subob))
                (if result (return result)))))))

(defun mst-complete-p ()
 (let ((active-mating (matingstree-mating current-matingstree)))
  (if (mating-completep active-mating)
      (msg t "Mating is complete.")
      (let ((result (unfulfiled-ob current-obligation)))
        (if result
            (msg t "The obligation " result " is unfulfiled.")
            (progn (setf (mating-completep active-mating) t)
                   (msg t "Mating is complete.")))))))


(defun mst-quiet-complete-p ()
 (let ((active-mating (matingstree-mating current-matingstree)))
  (if (mating-completep active-mating)
      t
      (let ((result (unfulfiled-ob current-obligation)))
        (if result
            nil
            (progn (setf (mating-completep active-mating) t)
                   t))))))
       
(defmtreeop show-mating
   (mtree-alias mst-show-mating)
   (mtree-args 0)
   (mhelp "Show the connections in the mating associated with the current node."))

(defun mst-show-mating ()
     (msg t "Active mating: ")
     (mst-show-mating-real current-matingstree))

(defun mst-show-mating-real (matingstree)
 (let ((active-mating (matingstree-mating matingstree)))
  (if (consp (car (mating-clist active-mating)))
      (msg t (l (mating-clist active-mating)))
      (print-clist (mating-clist active-mating) (connections-array)))
  (if (mating-completep active-mating)
      (msg t "is complete."))))

(defmtreeop show-substs
   (mtree-alias mst-show-substs)
   (mtree-args 0)
   (mhelp "Show the substitution stack associated with a matingstree node.")) 

(defun mst-show-substs ()
 (let ((active-mating (matingstree-mating current-matingstree)))
  (if (mating-success-nodes active-mating)
      (print-subst-stack
	(node-subst-stack (car (mating-success-nodes active-mating))))
      (print-subst-stack (mating-utree active-mating)))))

;;;--------------------------------------------------------------------
(defmtreeop sib
  (mtree-alias mst-go-sib)
  (mtree-args 1)
  (mtree-move T)
  (mtree-print T)
  (mhelp "Go to the next sibling of this node."))

(defwffop mst-go-sib
  (argnames matingstree)
  (argtypes matingstree)
  (mhelp "Go to the next sibling of this node.")
  (resulttype matingstree))

(defun mst-go-sib (mtree)
  (let* ((parent (matingstree-parent mtree))
	 (sibs (if parent (matingstree-sons parent) nil))
	 (newsib (if (< 1 (length sibs)) (cycle-list mtree sibs (car sibs)) nil)))
    (if (not parent)
	(throwfail t "You are at the top of the matingstree." t)
      (if (not newsib) (throwfail t "This node has no siblings." t)
	(setq current-obligation (matingstree-obligation newsib))))
    newsib))

(defun cycle-list (name list preserved)
  (if (= 1 (length list)) preserved
    (if (eq (matingstree-name name) (matingstree-name (car list)))
	(cadr list)
      (cycle-list name (cdr list) preserved))))
;;;--------------------------------------------------------------------
(defmtreeop up
  (mtree-alias mst-go-up)
  (mtree-args 1)
  (mtree-move T)
  (mtree-print T)
  (mhelp "Go up one level."))

(defwffop mst-go-up
  (argnames matingstree)
  (argtypes matingstree)
  (mhelp "Go up one level in the matingstree.")
  (resulttype matingstree))

(defun mst-go-up (mtree)
  (let ((parent (matingstree-parent mtree)))
    (if parent (setq current-obligation (matingstree-obligation parent))
       (throwfail "You are at the top of the matingstree."))
    parent))

(defmtreeop d
  (mtree-alias mst-go-down)
  (mtree-args 2)
  (mtree-default (0))
  (mtree-move T)
  (mtree-print T)
  (mhelp "Go down one level. D <nth> means go down along the nth subnode.
Counting begins from 0. Without argument, D means go down along the 
leftmost subnode."))

(defwffop mst-go-down
  (argnames node matingstree)
  (argtypes symbol-or-integer matingstree)
  (mhelp "Go down one level in the matingstree.")
  (resulttype matingstree))

(defun mst-go-down (nth mtree)
  (let ((son (nth nth (matingstree-sons mtree))))
    (if son (setq current-obligation (matingstree-obligation son))
       (throwfail "The matingstree doesn't have the given branch."))
    son))

;;;--------------------------------------------------------------------------
(defmtreeop kill
  (mtree-alias mst-kill)
  (mtree-args 1)
  (mtree-print T)
  (mhelp "KILL <node> means to mark the given node and 
all nodes below it as dead."))

(defwffop mst-kill
  (argnames node)
  (argtypes symbol-or-integer)
  (mhelp "KILL <node> means to mark the given node 
and all nodes below it as dead.")
  (resulttype matingstree))

(defun mst-kill (name)
  (let ((namestring (complete-leaf-name name matingstree-name))
	(cur-mt-node current-matingstree))
    (if (or (eval (mtree-command-interpreter (list 'goto name)))
            (throwfail t "The matingstree doesn't have node " namestring "." t))
	(progn 
	  (setf (matingstree-dead current-matingstree) t)
	  (let ((mstsons (matingstree-sons current-matingstree)))
	    (unless (null mstsons) 
		    (dolist (son mstsons) (mst-kill (matingstree-name son)))))
	  (eval (mtree-command-interpreter (list 'goto (matingstree-name cur-mt-node))))))))

(defmtreeop resurrect
  (mtree-alias mst-resurrect)
  (mtree-args 1)
  (mtree-print T)
  (mhelp "RESURRECT <node> means to mark the given node and 
all nodes below it as alive."))

(defwffop mst-resurrect
  (argnames node)
  (argtypes symbol-or-integer)
  (mhelp "RESURRECT <node> means to mark the given node 
and all nodes below it as alive.")
  (resulttype matingstree))

(defun mst-resurrect (name)
  (let ((namestring (complete-leaf-name name matingstree-name))
	(cur-mt-node current-matingstree))
    (if (or (eval (mtree-command-interpreter (list 'goto name)))
            (throwfail t "The matingstree doesn't have node " namestring "." t))
	(progn 
	  (setf (matingstree-dead current-matingstree) nil)
	  (let ((mstsons (matingstree-sons current-matingstree)))
	    (unless (null mstsons) 
		    (dolist (son mstsons) (mst-resurrect (matingstree-name son)))))
	  (eval (mtree-command-interpreter (list 'goto (matingstree-name cur-mt-node))))))))


;;;----------------------------------------------------------------------
(defmtreeop goto
  (mtree-alias mst-goto)
  (mtree-args 2)
  (mtree-default (mstroot))
  (mtree-move T)
  (mtree-print T)
  (mhelp "GOTO <node> means to go to the given node. If <node> is not given,
it means to go to the root of the matingstree"))

(defwffop mst-goto
  (argnames node matingstree)
  (argtypes symbol-or-integer matingstree)
  (mhelp "Move to specified node in an matingstree.")
  (resulttype matingstree))

(defun mst-goto-list (namestring mtreelist)
  (if mtreelist 
      (let ((hxmtree (car mtreelist))
            (hxmtreelist (cdr mtreelist)))
        (if  (string= (symbol-name (matingstree-name hxmtree)) namestring)
             (progn (setq current-obligation (matingstree-obligation hxmtree))
                    hxmtree)
             (or (mst-goto-list namestring (matingstree-sons hxmtree)) 
                 (mst-goto-list namestring hxmtreelist))))))

(defun find-root-mtree (mtree)
  (let ((hxmtree (matingstree-parent mtree)))
    (if hxmtree (find-root-mtree hxmtree) mtree)))

(defun mst-goto (name mtree)
  (let ((namestring (complete-leaf-name name matingstree-name))
        (root (find-root-mtree mtree)))
    (if (eq name 'mstroot) 
        (progn (setq current-obligation (matingstree-obligation root)) root)
        (or (mst-goto-list namestring (list root))
            (throwfail "The matingstree doesn't have the node.")))))

(defmtreeop unify
  (mtree-alias mst-unify)
  (mhelp "Go into UNIFY toplevel and check the UTREE structure
associated with the current node in the matingstree. The 
unification tree associated with the mating is passed on 
to the unification top-level. Any changes made to this tree 
are destructive. Applicable only for a higher-order unification 
problem. Mainly use to check the UTREE structure."))


(defun mst-unify ()
 (let ((active-mating (matingstree-mating current-matingstree)))
  (unless (node-p (mating-utree active-mating))
	  (multiple-value-bind
	   (root-node subst-hash-table)
	   (initialize-utree
	    (mating-utree active-mating)
	    (free-vars-in-etree current-eproof))
	   (setf (mating-utree active-mating) root-node)
	   (setf (mating-subst-hash-table active-mating)
		 subst-hash-table)))
  (let ((root-node (mating-utree active-mating))
	(subst-hash-table (mating-subst-hash-table active-mating))
	(displaywff t)
	(printdepth printdepth)
	(ppwfflag ppwfflag)
	(nodestack nil)
	(prev-node nil)
	(current-topnode nil)
	(printtypes printtypes)
	)
    (declare (special displaywff printdepth ppwfflag  root-node nodestack
		      printtypes current-topnode subst-hash-table prev-node))
    (%catch% (progn (setq current-topnode root-node prev-node root-node)
		    (uniftop)
		    (throwfail "Unification aborted."))
	     (exit-inferior-top)))))


(defmacro mst-clist ()
   `(mating-clist (matingstree-mating current-matingstree)))

;-----------------------------------------------------
(defun obt-goto-list (namestring obtreelist)
  (if obtreelist 
      (let ((hxobtree (car obtreelist))
            (hxobtreelist (cdr obtreelist)))
        (if  (string= (symbol-name (obligation-name hxobtree)) namestring)
             hxobtree
             (or (obt-goto-list namestring (obligation-next hxobtree)) 
                 (obt-goto-list namestring hxobtreelist))))))

(defun find-root (obtree)
  (let ((hxobtree (obligation-last obtree)))
    (if hxobtree (find-root hxobtree) obtree)))

(defun obt-goto (name obtree)
  (let ((namestring (complete-leaf-name name obligation-name))
        (root (find-root obtree)))
    (if (eq name 'obtroot) 
        root
        (or (obt-goto-list namestring (list root))
            (throwfail t "The current obligation tree doesn't have node " name ".")))))

;;---------------------------------
(defun occurs-in-jform (lit-name jform)
   (case (jform-type jform)
         (conjunction (dolist (com (conjunction-components jform))
                              (if (occurs-in-jform lit-name com) (return T))))
         (disjunction (dolist (com (disjunction-components jform))
                              (if (occurs-in-jform lit-name com) (return T))))
         (universal (occurs-in-jform lit-name (universal-scope jform)))
         (literal (eq lit-name (literal-name jform)))
         (T (throwfail "Illegal type of jform."))))

;;;Notice the difference between flet and labels
(defun occurs-in-obtree (lit-name obtree)
  (let ((l (occurs-in-obtree-1 lit-name obtree))
	(d (obt-goto (get-default-ob) current-obligation)))
    (if (member d l) (cons d (remove-if #'(lambda (x) (eq x d)) l))
      l)))

(defun occurs-in-obtree-1 (lit-name obtree)
   (labels ((lit-occurs-in-obtree (x) 
              (if (obligation-closed x) nil
                  (if (null (obligation-next x))
                      (if (occurs-in-jform lit-name (obligation-jform x))
                          (list x) nil)
                      (apply #'append (mapcar #'lit-occurs-in-obtree (obligation-next x)))))))
	   (lit-occurs-in-obtree obtree)))

(defflag mt-default-ob-mate
  (flagtype symbol)
  (default 'lowest)
  (subjects mtree-top transmit)
  (mhelp "Determines how ADD-CONN chooses the default obligation for
the second literal of the given pair (it is possible that this literal
will occur several times on the path, in several different obligations).
Options are:
LOWEST : Chooses the obligation which lies lowest (i.e. furthest from the
root)
HIGHEST : Chooses the obligation nearest to the root (but not the root).
HI-LO : Finds the obligation which occurs lowest; this obligation was 
first added at some point in the matingstree. Then chooses the highest 
obligation which was added at the same point in the matingstree."))

(definfo lowest
  (mhelp "A setting for DEFAULT-OB-MATE.
When applying ADD-CONN to an mtree, choose the default obligation by
choosing the obligation which lies lowest (i.e. furthest from the
root)."))

(definfo hi-lo
  (mhelp "A setting for DEFAULT-OB-MATE.
When applying ADD-CONN to an mtree, choose the default obligation by
finding the obligation which occurs lowest; this obligation was 
first added at some point in the matingstree. Then chooses the highest 
obligation which was added at the same point in the matingstree."))

(defun occurs-in-obtree-path (lit-name obtree)
  (case mt-default-ob-mate
	(lowest (occurs-in-obtree-path-l lit-name obtree))
	(highest (occurs-in-obtree-path-h lit-name obtree))
	(hi-lo (occurs-in-obtree-path-hl lit-name obtree))
	(t (occurs-in-obtree-path-l lit-name obtree))))

(defun occurs-in-obtree-path-l (lit-name obtree)
   (let ((cmplit (intern (complete-leaf-name lit-name leaf-name))))
      (and obtree (or (and (occurs-in-jform cmplit (obligation-jform obtree)) 
                           obtree)
                      (occurs-in-obtree-path-l cmplit (obligation-last obtree))))))

(defun occurs-in-obtree-path-h (lit-name obtree &optional (ol nil))
  (if (null (obligation-last obtree))
      (occurs-in-obtree-path-h-1 lit-name (remove-if-not #'obligation-disjunction (cons obtree ol)))
    (occurs-in-obtree-path-h lit-name (obligation-last obtree) (cons obtree ol))))

(defun occurs-in-obtree-path-h-1 (lit-name obtreelist)
   (let ((cmplit (intern (complete-leaf-name lit-name leaf-name)))
	 (obtree (car obtreelist)))
      (and obtree (or (and (occurs-in-jform cmplit (obligation-jform obtree)) 
                           obtree)
                      (occurs-in-obtree-path-h-1 cmplit (cdr obtreelist))))))

(defun occurs-in-obtree-path-hl (lit-name obtree &optional (ol nil))
  (if (or (null (obligation-last obtree)) (null (obligation-from-expanding (obligation-last obtree))))
      (occurs-in-obtree-path-hl1 lit-name (reverse (cons obtree ol)))
    (occurs-in-obtree-path-hl lit-name (obligation-last obtree) (cons obtree ol))))

;by this point obtreelist is a list of all the obligations in the path, from the 
;leaf to the root in that order.
;now we want to find the first obligation in which the desired literal occurs, and then
;return the *last* obligation which has the same from-extending property as that.
(defun occurs-in-obtree-path-hl1 (lit-name obtreelist)
  (let ((cmplit (intern (complete-leaf-name lit-name leaf-name))))
    (and (car obtreelist) (or (and (occurs-in-jform cmplit (obligation-jform (car obtreelist)))
				   (occurs-in-obtree-path-hl2 cmplit (reverse
				    (remove-if-not #'(lambda (x) 
						       (eq (obligation-from-expanding (car obtreelist))
							   (obligation-from-expanding x))) obtreelist))))
			      (occurs-in-obtree-path-hl1 cmplit (cdr obtreelist))))))

(defun occurs-in-obtree-path-hl2 (lit-name obtreelist)
  (and (car obtreelist) (or (and (occurs-in-jform lit-name (obligation-jform (car obtreelist)))
				 (car obtreelist))
			    (occurs-in-obtree-path-hl2 lit-name (cdr obtreelist)))))

(defmtreeop pick
   (mtree-alias pick-lit)
   (mtree-args 2)
   (mtree-print T)
   (mhelp "Pick a leaf which you may try to mate with another later.
(MB: I think that PICK N behaves as though you had just 
added a connection to N, and generates the appropriate
obligations, without actually demanding another leaf to
connect with. I think.)"))

(defwffop pick-lit
  (argnames literal obligation)
  (argtypes leaftype symbol-or-integer)
  (defaultfns (lambda (x y)
                 (let* ((x1 (if (eq x '$) x (intern (complete-leaf-name x leaf-name))))
                        (y1 (if (eq y '$)
                                (and (neq '$ x) 
                                   (or (car (occurs-in-obtree x1 current-obligation)) 
                                        (throwfail "Sorry, no such a leaf " x1 " in unfulfiled obligations.")))
                                (let ((y2 (if (obligation-p y) y 
                                              (obt-goto y current-obligation))))
                                   (if (obligation-next y2)
                                       (throwfail "Node " y2 " is not a leaf in the obligation tree.") y2)))))
                    (list x1 y1))))
  (resulttype ignore)
  (mhelp "Pick a leaf which you may try to mate with another later."))
