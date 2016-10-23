;;; -*- Mode:LISP; Package:auto -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;


(in-package :auto)

(deffile mtree-duplication
  (part-of mst)
  (extension lsp)
  (mhelp "Defines quantifier duplication functions for matingstree."))

(defun mt-substituted-for (var exptms)
  (if (null exptms)
    nil
    (if (eq (exp-var-var (car exptms)) var) 
	(exp-var-subst (car exptms))
      (mt-substituted-for var (cdr exptms)))))

(defun duplicate-var-and-deepen-it (exp)
  (let* ((bdwff (get-shallow exp))
         (new-var* (funcall ren-var-fn (bindvar bdwff)))
         (new-var  (make-exp-var :var new-var* :subst (or (mt-substituted-for (bindvar bdwff) (expansion-terms exp)) new-var*)))
         (new-leaf (create-leaf-node 
                        (substitute-term-var
                         new-var (bindvar bdwff) (gdr bdwff))
                        (positive-p exp) 
                        (cons new-var (etree-free-vars exp)) exp))
         (pair (cons new-var exp)))
    (push pair
	  (eproof-free-vars-in-etree current-eproof))
    (pushnew pair
             (eproof-free-vars-in-etree master-eproof))
    (setf (expansion-terms exp) 
	  (nconc (expansion-terms exp) (list new-var)))
    (setf (expansion-prim-vars exp)
	  (nconc (expansion-prim-vars exp) (list (list new-var))))
    (setf (etree-components exp)
	  (nconc (etree-components exp)
		 (list new-leaf)))
    (update-status nil (car (last (etree-components exp)))
		   (etree-status* exp))
    (add-to-syms (car (last (etree-components exp)))
		 (car (etree-components exp)))
    (setq new-leaf (deepen-to-literals new-leaf))
    (name-mapping (car (etree-components exp)) new-leaf (length (etree-components exp)))))


(defun dup-if-needed (times exp)
  (if (null exp) 0
   (let ((hxlen (length (etree-components exp))))
       (if (<= hxlen times)
           (progn (duplicate-var-and-deepen-it exp) 
                  (cr-eproof-jform) 0)
           (- hxlen 1 times)))))

(defun name-mapping (etree co-etree counter)
  (let ((string (write-to-string counter)))
    (labels ((ftemp  (x y) 
               (if (and (leaf-p x) (leaf-p y))
                   (progn (setf (get (leaf-name x) 'copy-names) 
                                (cons (leaf-name y) (get (leaf-name x) 'copy-names)))
                          (setf (get (leaf-name y) 'penname) 
                                (concatenate 'string 
                                    (symbol-name (leaf-name x)) "-" string)))
                   (mapc #'ftemp (etree-components x) (etree-components y)))))
       (ftemp etree co-etree))))

;;;The chain contains at most one element
(defun disj-over-univ-jform (jform objform)
   (if (eq (jform-type objform) 'disjunction) objform
       (if (eq (jform-type objform) 'universal) nil
           (let (chain)
             (do ((son jform parent)
                  (parent (jform-parent jform) (jform-parent parent)))
                 ((eq son objform) chain)
                 (if (eq (jform-type parent) 'disjunction)
                     (push (cons son parent) chain)
                     (if (or (eq (jform-type parent) 'universal) (eq (jform-type parent) 'conjunction))
			 ; the right component of this fixes the THM50 bug??? MB Tue Feb 14 23:06:42 1995
                         (setq chain nil))))))))

(defun univ-ancestor-jform (jform objform)
   (if (eq (jform-type objform) 'universal) objform
       (let (ancestor)
         (do ((parent jform (jform-parent parent)))
             ((eq parent objform) ancestor)
             (if (eq (jform-type parent) 'universal)
                 (setq ancestor parent))))))

               
(defun univ-ancestor-etree (etree bindvar)
  (let (ancestor)
    (do ((parent (etree-parent etree) (etree-parent parent)))
        ((null parent) ancestor)
        (if (and (expansion-p parent) 
		 (memq bindvar (mapcar #'(lambda (x) (exp-var-var (car x))) (expansion-prim-vars parent))))
            (setq ancestor parent)))))

;;;argument univ must be a universal jform
(defun univ-used-times (univ ob)
  (let ((objform (obligation-jform ob)))
   (case (jform-type objform) 
     (universal (if (eq univ objform) (car (obligation-used-univs ob))))
     (conjunction 
        (let ((pos (position univ (conjunction-components objform))))
          (if pos (elt (obligation-used-univs ob) pos) 1)))
     (t 1))))

;;;leaf-name is a special variable.
(defun update-current-etree-1 (leaf-name-arg ob)
   (let ((leaf (find-etree-node-name leaf-name-arg current-topnode))
         (leaf-ob (find-jform-symbol leaf-name-arg (obligation-jform ob)))
         (true-name leaf-name-arg))
     (if leaf
         (let ((new-jform (univ-ancestor-jform leaf-ob (obligation-jform ob))))
            (when new-jform
                  (let ((nth (dup-if-needed
                                (univ-used-times new-jform ob)
                                (univ-ancestor-etree leaf (car (universal-qvars new-jform))))))
;;;                    (cr-eproof-jform)
                    (elt (get leaf-name-arg 'copy-names) nth))))            
          (throwfail "Node " leaf-name-arg " does not occur in current-topnode."))))

(defun update-current-etree-2 (leaf-name-1 ob1 leaf-name-2 ob2)
   (let ((leaf-1 (find-etree-node-name leaf-name-1 current-topnode))
         (leaf-2 (find-etree-node-name leaf-name-2 current-topnode))
         (leaf-ob-1 (find-jform-symbol leaf-name-1 (obligation-jform ob1)))
         (leaf-ob-2 (find-jform-symbol leaf-name-2 (obligation-jform ob2)))
         (true-name-1 leaf-name-1) (true-name-2 leaf-name-2))
     (if (and leaf-1 leaf-2)
         (let ((new-jform-1 (univ-ancestor-jform leaf-ob-1 (obligation-jform ob1)))
               (new-jform-2 (univ-ancestor-jform leaf-ob-2 (obligation-jform ob2))))
            (when new-jform-1
                  (let ((nth (dup-if-needed
                                (univ-used-times new-jform-1 ob1)
                                (univ-ancestor-etree leaf-1 (car (universal-qvars new-jform-1))))))
                     (setq true-name-1 (if (get leaf-name-1 'copy-names)
					   (elt (get leaf-name-1 'copy-names) nth)
					 leaf-name-1))))
            (when new-jform-2
                  (let* ((nth (dup-if-needed 
                                (if (eq new-jform-1 new-jform-2)
                                    (1+ (univ-used-times new-jform-2 ob2))
				  (univ-used-times new-jform-2 ob2))
                                (univ-ancestor-etree leaf-2 (car (universal-qvars new-jform-2))))))
                     (setq true-name-2 (if (get leaf-name-2 'copy-names)
					   (elt (get leaf-name-2 'copy-names) nth)
					 leaf-name-2))))
            (if (or new-jform-1 new-jform-2) (cr-eproof-jform))
;above line was commented out MB
            (cons true-name-1 true-name-2))
         (throwfail "Node " leaf-name-1 " or Node " leaf-name-2 " does not occur in current-topnode."))))
               

(defun find-jform-symbol (symbol jform)
   (case (jform-type jform)
    (literal
     (if (eq (literal-name jform) symbol) jform nil))
    (conjunction
     (dolist (jform (conjunction-components jform) nil)
       (let ((literal (find-jform-symbol symbol jform)))
         (if literal (return literal)))))
    (disjunction
     (dolist (jform (disjunction-components jform) nil)
       (let ((literal (find-jform-symbol symbol jform)))
         (if literal (return literal)))))
    (universal (find-jform-symbol symbol (universal-scope jform)))
    (t (throwfail (jform-type jform) " is illegal in jforms."))))

;;; ob must be of type conjunction or universal
(defun update-used-univs (univ ob)
  (let* ((objform (obligation-jform ob)))
    (and univ (case (jform-type objform)
                    (conjunction 
                       (let* ((comlist (conjunction-components objform))
                              (pos (position univ comlist))
                              (remlist (if pos (nthcdr pos (obligation-used-univs ob)))))
                          (if pos (rplaca remlist (1+ (car remlist))))))
                    (universal 
                          (if (eq univ objform) 
                              (let ((single (obligation-used-univs ob)))
                                 (rplaca single (1+ (car single))))))))))

(defun pick-lit (newsym ob)
   (let* ((tlit-name (update-current-etree-1 newsym ob))
          (lit (find-jform-name newsym (obligation-jform ob)))
          (tlit (find-jform-name tlit-name (eproof-jform current-eproof)))
          (active-mating 
               (hxcopy-mating (matingstree-mating current-matingstree)))
          (chain (chain lit (obligation-jform ob) tlit)))
    (if (null chain) 
        (throwfail "It is not necessary to pick " newsym ".")
        (progn (setq current-obligation (copy-obligation-true current-obligation))
               (let* ((outer-univ (univ-ancestor-jform lit (obligation-jform ob)))
                      (newob (obt-goto (obligation-name ob) current-obligation)))
                 (expand-obtree chain newob
                      (if (update-used-univs outer-univ newob) nil outer-univ)))
               (update-used-univs lit (obt-goto (obligation-name ob) current-obligation))
               (let ((leaf (make-matingstree :mating active-mating
                                             :obligation current-obligation)))
                (extend-leaf leaf current-matingstree)
                (setf current-matingstree leaf)
		)))))

(defun add-conn-ob (newsym1 ob1 newsym2 ob2)
   (let* ((last-jform (eproof-jform current-eproof))
          (pair (update-current-etree-2 newsym1 ob1 newsym2 ob2))
          (jform (eproof-jform current-eproof))
          (lit1 (find-jform-name newsym1 (obligation-jform ob1)))
          (lit2 (find-jform-name newsym2 (obligation-jform ob2)))
          (tlit1 (find-jform-name (car pair) jform))
          (tlit2 (find-jform-name (cdr pair) jform))
          (active-mating 
               (hxcopy-mating (matingstree-mating current-matingstree)))
;	  (rubbish2 (inspect active-mating)) ; for debugging
	  (mate-ffpair t)
          (connection (find-potential-connection
		      tlit1 (list tlit2) (cgraph)
		      (mating-clist active-mating)))
          (hxmember (and connection (member connection (mating-clist active-mating))))
          (hxclist  (and connection
                         (sort (cons connection (copy (mating-clist active-mating))) #'<)))
	  (mbsubsumed nil)
	  (newtag nil)
          (obname1 (obligation-name ob1))
          (obname2 (obligation-name ob2))
          (chain1 (chain lit1 (obligation-jform ob1) tlit1))
          (chain2 (chain lit2 (obligation-jform ob2) tlit2))
	  #+comment(rubbish (msg "Connection: " connection t "Hxmember: " hxmember t 
			"Last-jform: " last-jform t "Pair: " pair t "jform: " jform t
			"lit1: " lit1 t "lit2: " lit2 t "tlit1: " tlit1 t "tlit2: " tlit2 t
			"active-mating: " active-mating t 
			"connection: " connection t
			"hxclist: " hxclist t "obname1: " obname1 t "obname2: " obname2 t
			"chain1: " chain1 t "chain2: " chain2 t))
          (add-in (and connection (not hxmember) 
                      (progn (add-connection connection active-mating)
                             (if first-order-mode-ms
		                 (multiple-value-bind (flag substs)
			             (fo-compatible-p connection
					              (mating-utree active-mating))
		                   (unless (eq flag 'fail)
			             (setf (mating-utree active-mating) substs))
		                   (neq flag 'fail))
		                 (multiple-value-bind (flag root subst-hash-table
					                    success-nodes)
			              (unifiable-p active-mating t)
		                   (setf (mating-utree active-mating) root)
		                   (setf (mating-subst-hash-table active-mating)
			                 subst-hash-table)
		                   (setf (mating-success-nodes active-mating) success-nodes)
		                   (when (eq flag 'fail)
			              (modify-utree connection active-mating))
		                   (neq flag 'fail)))))))
     (multiple-value-setq (mbsubsumed newtag) (mtree-subsumption-check hxclist))
     (if mbsubsumed 
	 (progn
	   (msgf "Not adding this connection; the result is subsumed by " (car mbsubsumed) "." t)
	   (setf (eproof-jform current-eproof) last-jform))
       (progn
	 (when (or (and hxmember (not mtree-filter-dups)) add-in) ;was or hxmember add-in
	       (setf current-obligation (copy-obligation-true current-obligation))
	       (let* ((newob1 (obt-goto obname1 current-obligation))
		      (newchain1 chain1) 
		      (outer-univ1 (univ-ancestor-jform lit1 (obligation-jform ob1)))
		      (succ1 (update-used-univs outer-univ1 newob1))
		      (newob (expand-obtree newchain1 newob1 (if succ1 nil outer-univ1)))
		      (newchain2 chain2)
		      (outer-univ2 (univ-ancestor-jform lit2 (obligation-jform ob2)))
		      (succ2 (update-used-univs outer-univ2
						(obt-goto obname2 current-obligation)))
		      (newob (expand-obtree newchain2 newob (if succ2 nil outer-univ2))))
		 (setf-obligation-closed newob (cons newsym1 newsym2))))
	 (if connection
	     (if hxmember 
		 (if (not mtree-filter-dups);was progn
		     (let ((leaf (make-matingstree 
				  :mating active-mating
				  :obligation current-obligation
				  :literal-pair (cons newsym1 newsym2))))
		       (extend-leaf leaf current-matingstree)
		       (setf current-matingstree leaf))
		   (progn 
		     (unless (eq mating-verbose 'silent) (msg t "The connection has been added already."))
		     (setf (eproof-jform current-eproof) last-jform)))
	       (if add-in 
		   (progn (let ((leaf (make-matingstree 
				       :mating active-mating
				       :obligation current-obligation
				       :literal-pair (cons newsym1 newsym2))))
			    (extend-leaf leaf current-matingstree)
			    (setf current-matingstree leaf))
;;;                         (update-used-univs lit1 (obt-goto obname1 current-obligation))
;;;                         (update-used-univs lit2 (obt-goto obname2 current-obligation))
			  (push (cons (cons (matingstree-name current-matingstree) newtag) hxclist) 
				already-added-clists)
			  (unless (eq mating-verbose 'silent) 
				  (msg T "Adding new connection: ")
				  (print-connection connection (connections-array)))
			  'success)
		 (progn (unless (eq mating-verbose 'silent)
				(msg t "Adding this connection causes incompatibility." t
				     "Connection not added.")
				(setf (eproof-jform current-eproof) last-jform))
			(setf (incomp-clists)
			      (store-incomp-clist (mating-clist active-mating)
						  (incomp-clists)))
			(remove-last-connection active-mating))))
	   (progn (setf (eproof-jform current-eproof) last-jform)
		  (complain "Cannot add this connection: " newsym1 " " newsym2)))))))

(defflag mt-subsumption-check
  (flagtype mt-subsumption)
  (default 'same-conns)
  (subjects mtree-top transmit)
  (mhelp "If SAME-CONNS or T, will check whether the node about to be added is
duplicated elsewhere in the tree, and will reject it if it is. (This will use
the SAME-TAG function described below, and then do a more thorough check if the 
tags match.)

If SUBSET-CONNS, will check whether the connections at the node about to be
added are a subset of those at some other node. (This is only really useful in
MT94-11, where all possible new nodes are added, breadth-first, to the tree.
It is probably too restrictive for the other mtree searches.)

If SAME-TAG will check whether the tag (an integer generated from the list of 
connections) is the same as any other existing tag, and will reject it if it is.
See TAG-CONN-FN and TAG-LIST-FN. (Note that most tag functions can produce the
same tag for different matings, so this may reject connections unnecessarily.)

If NIL, will turn off subsumption checking altogether."))

(definfo same-conns
  (mhelp "A setting for MT-SUBSUMPTION-CHECK.
Will check whether the mtree node about to be added is
duplicated elsewhere in the tree, and will reject it if it is. (This will use
the SAME-TAG function, and then do a more thorough check if the 
tags match.)"))

(definfo subset-conns
  (mhelp "A setting for MT-SUBSUMPTION-CHECK.
Will check whether the connections at the mtree node about to be
added are a subset of those at some other node. (This is only really useful in
MT94-11, where all possible new nodes are added, breadth-first, to the tree.
It is probably too restrictive for the other mtree searches.)"))

(definfo same-tag
  (mhelp "A setting for MT-SUBSUMPTION-CHECK.
Will check whether the tag of the mtree node about to be added (an integer 
generated from the list of connections) is the same as any other existing tag, 
and will reject it if it is. See TAG-CONN-FN and TAG-LIST-FN. (Note that most 
tag functions can produce the same tag for different matings, so this may 
reject connections unnecessarily.)"))

(defflag tag-conn-fn
  (flagtype symbol)
  (default 'tag-conn-leafno)
  (subjects mtree-top)
  (mhelp "Determines how the tag (a number attached to each mating) is calculated.
Should be the name of a function which, given a connection, will generate an
integer from it. See MT-SUBSUMPTION-CHECK and TAG-MATING-FN.

Current settings are 
TAG-CONN-QUICK, which uses TPS's internal number for the connection. (Actually,
it uses (1 + this number), so as to avoid multiplying by one.)
TAG-CONN-LEAFNO, which multiplies the integer parts of the two leaf names
in the given connection."))

(definfo tag-conn-quick
  (mhelp "A setting for TAG-CONN-FN.
Given a connection, return TPS's internal number for the connection. (Actually,
it uses (1 + this number), so as to avoid multiplying by one.)"))

(definfo tag-conn-leafno
  (mhelp "A setting for TAG-CONN-FN.
Given a connection, return the product of the integer parts of the two leaf names
in the given connection."))

(defflag tag-mating-fn
  (flagtype symbol)
  (default 'multiply-tag-list)
  (subjects mtree-top)
  (mhelp "Determines how the tags for each connection are combined to produce
a tag for the entire mating.
Should be the name of a function which, given two integers, will generate a
third integer. See MT-SUBSUMPTION-CHECK and TAG-MATING-FN.

Current settings are 
MULTIPLY-TAG-LIST, which simply multiplies the numbers together."))

(definfo multiply-tag-list
  (mhelp "A setting for TAG-MATING-FN.
Given a list of tags for connections, multiply them together to get
a tag for the mating."))

(defun mtree-subsumption-check (newclist)
  (if mt-subsumption-check
      (case mt-subsumption-check
	    (same-tag (let ((newtag (mtree-tag-clist newclist)))
			(values (mtree-subsumption-check-tag newtag already-added-clists) newtag)))
	    (subset-conns   (values (mtree-subsumption-check-subset newclist already-added-clists) nil))
	    ((t same-conns) (let ((newtag (mtree-tag-clist newclist)))
			      (values (mtree-subsumption-check-real newclist newtag already-added-clists) newtag)))
  )
    (values nil nil)))

(defun mtree-subsumption-check-real (newclist newtag &optional (oldclist already-added-clists))
  (if (null oldclist) nil
    (if (or (neq (cdaar oldclist) newtag) ;check they have the same tag no.
	    (neq (length (cdar oldclist)) (length newclist)) ;check they're the same length
	    (set-exclusive-or newclist (cdar oldclist)))      ;finally check they're actually the same.
	(mtree-subsumption-check-real newclist newtag (cdr oldclist))
      (caar oldclist))))

(defun mtree-subsumption-check-tag (newtag &optional (oldclist already-added-clists))
  (if (null oldclist) nil
    (if (eq newtag (cdaar oldclist))
	(caar oldclist)
      	(mtree-subsumption-check-tag newtag (cdr oldclist))
	)))

(defun mtree-subsumption-check-subset (newclist &optional (oldclist already-added-clists))
  (if (null oldclist) nil
    (if (subsetp newclist (cdar oldclist))
	(caar oldclist)
      	(mtree-subsumption-check-subset newclist (cdr oldclist))
	)))

(defun mtree-tag-clist (clist)
  ;given a mating, generates a (not necessarily unique) number to attach to it.
  ;this will probably speed up subsumption checking.
  ;function mtree-tag-conn generates a number for each connection
  ;function mtree-tag-list combines these numbers pairwise to produce a final result.
  ;Both of these functions should be commutative and associative.
  (let ((i 1)) ;the id number of the empty clist
    (dolist (conn clist)
	    (setq i (funcall tag-mating-fn (funcall tag-conn-fn conn) i)))
    i))

(defun multiply-tag-list (i j)
  (* i j))

(defun tag-conn-quick (conn)
  (1+ conn))
;connections all have a number

(defun tag-conn-leafno (conn)
  (let* ((conn (car (gethash conn (connections-array))))
	 (conn (cons (show-prop-name (car conn)) (show-prop-name (cdr conn))))
	 (i (string-left-trim (princ-to-string leaf-name) (princ-to-string (car conn))))
	 (i (if (rassoc (car conn) *renamed-leaves-list*)
		(string-right-trim "." (string-right-trim "0123456789" i))
	      i))
	 (j (string-left-trim (princ-to-string leaf-name) (princ-to-string (cdr conn))))
	 (j (if (rassoc (cdr conn) *renamed-leaves-list*)
		(string-right-trim "." (string-right-trim "0123456789" j))
	      j)))
    ;all of that reduces LEAFXX.Y to "XX"
    (* (parse-integer i) (parse-integer j))))

(defun chain (offspring ancestor toffspring)
  (let* ((chain (disj-over-univ-jform offspring ancestor))
         (parent offspring) (tparent toffspring) 
         (ancestor (if chain (caar chain) ancestor))
         (hxchain nil))
    (do ((son parent parent)
         (tson tparent tparent))
        ((null son) (throwfail "The two jforms are not in a chain."))
        (if (eq son ancestor) (return (append chain hxchain)))
        (setq parent (jform-parent son))
        (setq tparent (jform-parent tson))
        (if (eq (jform-type tparent) 'disjunction)  ; was (eq (jform-type parent) 'disjunction)
                (push (cons tson tparent) hxchain)))))

;;;The following function should be used after knowing son is a component
;;; of parent.
(defun whichone (son parent)
   (case (jform-type parent)
      (disjunction (position son (disjunction-components parent)))
      (conjunction (position son (conjunction-components parent)))
      ((universal literal) 
            (throwfail "The second jform is not a disjunction or conjunction."))))

(defun expand-obtree (chain ob &optional outer-univ)
  (let* ((outer (if (and outer-univ (conjunction-p (jform-parent outer-univ)))
                    (jform-parent outer-univ)
                    outer-univ))
         (update outer-univ))
    (do ((hxchain chain (cdr hxchain))
         (newob ob newob))
        ((null hxchain) newob)
	(setq newob 
          (disj-block (whichone (caar hxchain) (cdar hxchain))
                      (cdar hxchain)  newob))
      (when (and update (eq (obligation-jform newob) outer))
            (update-used-univs outer-univ newob)
            (setq update nil)))))

;;;The following function is introduced because of sharing of 
;;;datastructure between a copy of obligation and its copy 
;;;obtained by using function copy-obligation.
(defun copy-obligation-true (ob)
  (let* ((hxob (copy-obligation ob))
         (hxnext (obligation-next hxob)))
    (setf (obligation-used-univs hxob) (copy-list (obligation-used-univs ob)))
    (if hxnext 
        (let ((hxnewnext (mapcar #'copy-obligation-true hxnext)))
             (setf (obligation-next hxob) hxnewnext)
             (dolist (newnext hxnewnext)
                     (setf (obligation-last newnext) hxob))
             hxob)
        hxob)))

