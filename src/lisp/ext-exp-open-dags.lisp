;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
(part-of EXT-DAGS)

;;;
;;; File: EXT-EXP-OPEN-DAGS  - cebrown - 4/03

(deffile ext-exp-open-dags
    (part-of EXT-DAGS)
  (extension clisp)
  (mhelp "Open Extensional Expansion Dags (i.e., with expansion variables)"))

(context ext-exp-dags)

(defun print-ext-exp-open-dag (d s k)
  (declare (ignore k))
  (let ((*standard-output* (make-string-output-stream)))
    (if ext-exp-dag-verbose
	(print-ext-exp-open-dag-verbose d)
      (format t "~d " (ext-exp-open-dag-name d)))
    (let ((str (get-output-stream-string *standard-output*)))
      (write-string str s)
      (close *standard-output*))))

(defun print-ext-exp-open-arc (d s k)
  (declare (ignore k))
  (let ((*standard-output* (make-string-output-stream)))
    (if ext-exp-dag-verbose
	(print-ext-exp-arc-verbose d)
      (case (ext-exp-open-arc-kind d)
	(EXP (msg "Exp[" ((ext-exp-open-arc-exp-term d) . gwff) "]"))
	(SEL (msg "Sel[" ((ext-exp-open-arc-sel-var d) . gwff) "]"))
	(MATE "Mate")
	(EUNIF1 "EU1")
	(EUNIF2 "EU2")
	(otherwise "Arc")))
    (let ((str (get-output-stream-string *standard-output*)))
      (write-string str s)
      (close *standard-output*))))

(defun print-ext-exp-open-arc-verbose (d)
  (let ((printed-nodes nil)
	(print-shallows nil))
    (declare (special printed-nodes print-shallows))
    (print-ext-exp-open-arc-verbose-rec d)))

(defun print-ext-exp-open-dag-verbose (d)
  (let ((printed-nodes nil)
	(print-shallows nil))
    (declare (special printed-nodes print-shallows))
    (print-ext-exp-open-dag-verbose-rec d)))

(defun print-ext-exp-open-arc-verbose-rec (d)
  (let ((n (ext-exp-open-arc-node d)))
    (if (ext-exp-open-arc-exp-term d)
	(msgf "  EXP: " ((ext-exp-open-arc-exp-term d) . gwff) " --> " (ext-exp-open-dag-name n))
      (if (ext-exp-open-arc-sel-var d)
	  (msgf "  SEL: " ((ext-exp-open-arc-sel-var d) . gwff) " --> " (ext-exp-open-dag-name n))
	(if (eq (ext-exp-open-arc-kind d) 'DEC)
	    (msgf "  -DEC" (ext-exp-open-arc-dec-index d) "-> " (ext-exp-open-dag-name n)))))
    (print-ext-exp-open-dag-verbose-rec n)))

(defun print-ext-exp-open-dag-verbose-rec (d)
  (declare (special printed-nodes print-shallows))
  (unless (member d printed-nodes)
    (push d printed-nodes)
    (if (or print-shallows
	    (member (ext-exp-open-dag-kind d) '(ATOM EQN FLEX)))
	(msgf (ext-exp-open-dag-name d) " "
	      (if (ext-exp-open-dag-positive d) "+" "-") ":"
	      (or (ext-exp-open-dag-rew-just d)
		  (ext-exp-open-dag-kind d))
	      " " ((ext-exp-open-dag-shallow d) . gwff) t)
      (msgf (ext-exp-open-dag-name d) " "
	    (if (ext-exp-open-dag-positive d) "+" "-") ":"
	    (or (ext-exp-open-dag-rew-just d)
		(ext-exp-open-dag-kind d))))
    (when (ext-exp-open-dag-arcs d)
      (format t "   ->")
      (dolist (z (ext-exp-open-dag-arcs d))
	(if (ext-exp-open-arc-kind z)
	    (format t " (~d)~d" 
		    (ext-exp-open-arc-kind z)
		    (ext-exp-open-dag-name (ext-exp-open-arc-node z)))
	  (format t " ~d" (ext-exp-open-dag-name (ext-exp-open-arc-node z))))))
    (dolist (z (ext-exp-open-dag-arcs d))
      (print-ext-exp-open-arc-verbose-rec z))))

(defun set-eeod-true (node)
  (setf (ext-exp-open-dag-kind node) 'TRUE)
  (setf (ext-exp-open-dag-name node) (intern-str (create-namestring true-name)))
  node)

(defun set-eeod-false (node)
  (setf (ext-exp-open-dag-kind node) 'FALSE)
  (setf (ext-exp-open-dag-name node) (intern-str (create-namestring false-name)))
  node)

(defun set-eeod-neg (node eea)
  (setf (ext-exp-open-dag-kind node) 'NEG)
  (setf (ext-exp-open-dag-name node) (intern-str (create-namestring neg-name)))
  (setf (ext-exp-open-dag-arcs node) (list eea))
  node)

(defun set-eeod-con (node eea1 eea2)
  (setf (ext-exp-open-dag-kind node) 'CON)
  (setf (ext-exp-open-dag-name node) (intern-str (create-namestring econj-name)))
  (setf (ext-exp-open-dag-arcs node) (list eea1 eea2))
  node)

(defun set-eeod-dis (node eea1 eea2)
  (setf (ext-exp-open-dag-kind node) 'DIS)
  (setf (ext-exp-open-dag-name node) (intern-str (create-namestring edisj-name)))
  (setf (ext-exp-open-dag-arcs node) (list eea1 eea2))
  node)

(defun set-eeod-imp (node eea1 eea2)
  (setf (ext-exp-open-dag-kind node) 'IMP)
  (setf (ext-exp-open-dag-name node) (intern-str (create-namestring imp-name)))
  (setf (ext-exp-open-dag-arcs node) (list eea1 eea2))
  node)

(defun set-eeod-sel (node arcs)
  (setf (ext-exp-open-dag-kind node) 'SEL)
  (setf (ext-exp-open-dag-name node) (intern-str (create-namestring selection-name)))
  (setf (ext-exp-open-dag-arcs node) arcs)
  node)

(defun set-eeod-exp (node arcs)
  (setf (ext-exp-open-dag-kind node) 'EXP)
  (setf (ext-exp-open-dag-name node) (intern-str (create-namestring expansion-name)))
  (setf (ext-exp-open-dag-arcs node) arcs)
  node)

(defun set-eeod-rew (node rew-just eeoa)
  (setf (ext-exp-open-dag-kind node) 'REW)
  (setf (ext-exp-open-dag-rew-just node) rew-just)
  (setf (ext-exp-open-dag-name node) (intern-str (create-namestring rewrite-name)))
  (setf (ext-exp-open-dag-arcs node) (list eeoa))
  node)

(defun set-eeod-eqo (node eeoa)
  (set-eeod-rew node 'EXT= eeoa))

(defun set-eeod-exto (node eeoa)
  (set-eeod-rew node 'EXT= eeoa))

(defun set-eeod-eqn (node)
  (let ((name (intern-str (create-namestring 'EQN))))
    (setf (ext-exp-open-dag-kind node) 'EQN)
    (setf (ext-exp-open-dag-name node) name)
    (setf (get name 'ext-exp-open-dag) node)))

(defun set-eeod-atom (node)
  (let ((name (intern-str (create-namestring leaf-name))))
    (setf (ext-exp-open-dag-kind node) 'ATOM)
    (setf (ext-exp-open-dag-name node) name)
    (setf (get name 'ext-exp-open-dag) node)))

(defun set-eeod-flex (node)
  (let ((name (intern-str (create-namestring leaf-name))))
    (setf (ext-exp-open-dag-kind node) 'FLEX)
    (setf (ext-exp-open-dag-name node) name)
    (setf (get name 'ext-exp-open-dag) node)))

(defun copy-eeod (eeod)
  (let ((eeod2 (copy-ext-exp-open-dag eeod))
	(name2 (intern-str (create-namestring
			    (read-from-string
			     (string-right-trim "1234567890"
						(ext-exp-open-dag-name eeod)))))))
    (setf (ext-exp-open-dag-name eeod2) name2)
    (setf (get name2 'ext-exp-open-dag) eeod2)
    (setf (ext-exp-open-dag-stamp eeod2)
	  (incf *eeod-stamp*))
    eeod2))

(defun copy-eeod-rec (eeod)
  (let ((node-assoc nil))
    (declare (special node-assoc))
    (copy-eeod-rec-1 eeod)))

(defun copy-eeod-rec-1 (eeod)
  (declare (special node-assoc))
  (let ((a (assoc eeod node-assoc)))
    (if a
	(cdr a)
      (let ((ceeod (copy-ext-exp-open-dag eeod)))
	(setf (ext-exp-open-dag-arcs ceeod)
	      (mapcar #'(lambda (arc)
			  (copy-eeoa-rec-1 arc))
		      (ext-exp-open-dag-arcs eeod)))
	(push (cons eeod ceeod) node-assoc)
	ceeod))))

(defun copy-eeoa-rec-1 (arc)
  (let ((carc (copy-ext-exp-open-arc arc)))
    (setf (ext-exp-open-arc-node carc)
	  (copy-eeod-rec-1 (ext-exp-open-arc-node arc)))
    carc))

(defun name-to-eeod (name)
  (let ((node (get name 'ext-exp-open-dag))
	(done nil))
    (unless (ext-exp-open-dag-p node)
      (throwfail "No node associated with " name))
    (loop until (or done (eq (ext-exp-open-dag-name node) name)) do
	  (setq name (ext-exp-open-dag-name node))
	  (if (get name 'ext-exp-open-dag)
	      (setq node (get name 'ext-exp-open-dag))
	    (setq done t)))
    node))

(defun eeod-junctive (node)
  (case (ext-exp-open-dag-kind node)
    ((CON TRUE) (if (ext-exp-open-dag-positive node) 'CON 'DIS))
    ((DIS IMP FALSE) (if (ext-exp-open-dag-positive node) 'DIS 'CON))
    (EXP 'CON)
    (DEC 'DIS)
    (EQN (if (ext-exp-open-dag-positive node) nil 'CON))
    (t nil)))

(defun ext-exp-var-p (x)
  (and (symbolp x)
       (get x 'ext-exp-var)))

(defun push-simple-eqndec-arc (wff ret)
  (let ((simple-edag t))
    (declare (special simple-edag))
    (push-eqndec-arc wff ret)))

(defun push-basic-eqndec-arc (wff ret)
  (let ((simple-edag nil))
    (declare (special simple-edag))
    (push-eqndec-arc wff ret)))

(defun push-eqndec-arc (wff ret)
  (let* ((lft (cdar wff))
	 (rght (cdr wff))
	 (hl (head lft))
	 (hr (head rght)))
    (when (and (eq hl hr) (not (ext-exp-var-p hl))
	       (not (find-if #'(lambda (arc) (eq (ext-exp-open-arc-kind arc) 'EQNDEC))
			     (ext-exp-open-dag-arcs ret))))
      (let ((arc2 (make-ext-exp-open-arc :kind 'EQNDEC))
	    (eqndec (make-ext-exp-open-dag))
	    (n (length (args lft))))
	(push arc2 (ext-exp-open-dag-arcs ret))
	(setf (ext-exp-open-arc-parent arc2) ret)
	(setf (ext-exp-open-arc-node arc2) eqndec)
	(setf (ext-exp-open-dag-parent-arcs eqndec) (list arc2))
	(setf (ext-exp-open-dag-kind eqndec) 'DEC)
	(setf (ext-exp-open-dag-name eqndec) (intern-str (create-namestring 'DEC)))
	(setf (ext-exp-open-dag-shallow eqndec) wff)
	(setf (ext-exp-open-dag-positive eqndec) nil)
	(setf (ext-exp-open-dag-arcs eqndec)
	      (create-ext-exp-open-arc-decs n lft rght eqndec))
	eqndec))))

(defun create-ext-exp-open-dag (wff &optional (simple t))
  (declare (special *current-edag* *current-edag-lemmas* *current-edag-lemma-ftree-pfs* *first-equiv*))
  (setq *current-edag-lemmas* nil)
  (setq *current-edag-lemma-ftree-pfs* nil)
  (setq *first-equiv* t)
  (let ((simple-edag simple))
    (declare (special simple-edag))
    (let ((eeod (create-ext-exp-open-dag-1 wff nil)))
      (setq *current-edag* eeod))))

(defun create-simple-ext-exp-open-dag (wff &optional pos parentarcs)
  (let ((simple-edag t))
    (declare (special simple-edag))
    (create-ext-exp-open-dag-1 wff pos parentarcs)))

(defun create-basic-ext-exp-open-dag (wff &optional pos parentarcs)
  (let ((simple-edag nil))
    (declare (special simple-edag))
    (create-ext-exp-open-dag-1 wff pos parentarcs)))

(defun create-ext-exp-open-dag-1 (wff &optional pos parentarcs)
  (create-ext-exp-open-dag-2 (lazy-abbrev-normalize wff) pos parentarcs))

(defun create-ext-exp-open-dag-2 (wff &optional pos parentarcs)
  (declare (special simple-edag))
  (let ((ret (make-ext-exp-open-dag :parent-arcs parentarcs
				    :shallow wff
				    :positive pos)))
    (cond ((eq wff 'TRUTH)
	   (set-eeod-true ret))
	  ((eq wff 'FALSEHOOD)
	   (set-eeod-false ret))
	  ((not-p wff)
	   (set-eeod-neg ret (create-ext-exp-open-arc-1 (cdr wff) (not pos) ret)))
	  ((or-p wff)
	   (set-eeod-dis
	    ret
	    (create-ext-exp-open-arc-1 (cdar wff) pos ret)
	    (create-ext-exp-open-arc-1 (cdr wff) pos ret)))
	  ((and-p wff)
	   (set-eeod-con
	    ret
	    (create-ext-exp-open-arc-1 (cdar wff) pos ret)
	    (create-ext-exp-open-arc-1 (cdr wff) pos ret)))
	  ((implies-p wff)
	   (set-eeod-imp
	    ret
	    (create-ext-exp-open-arc-1 (cdar wff) (not pos) ret)
	    (create-ext-exp-open-arc-1 (cdr wff) pos ret)))
	  ((equiv-p wff)
	   (let ((rewrite-as-conj
		  (case rewrite-equivs
		    (2 pos)
		    (3 (if *first-equiv* (not pos) pos))
		    (4 t)
		    (5 nil)
		    (t (not pos)))))
	     (setq *first-equiv* nil)
	     (if rewrite-as-conj
		 (set-eeod-rew
		  ret 'EQUIV-IMPLICS
		  (create-ext-exp-open-arc-1
		   (acons 'AND
			  (acons 'IMPLIES (cdar wff) (cdr wff))
			  (acons 'IMPLIES (cdr wff) (cdar wff)))
		   pos ret))
	       (set-eeod-rew
		ret 'EQUIV-DISJS
		  (create-ext-exp-open-arc-1
		   (acons 'OR
			  (acons 'AND (cdar wff) (cdr wff))
			  (acons 'AND
				 (cons 'NOT (cdar wff))
				 (cons 'NOT (cdr wff))))
		   pos ret)))))
	  ((or (and pos (a-bd-wff-p wff))
	       (and (not pos) (e-bd-wff-p wff)))
	   (if simple-edag ; building simple, with exp vars
	       (let* ((x (bindvar wff))
		      (ev (fresh-var (unabbreviated-type x) (getnameroot x))))
		 (set-eeod-exp
		  ret
		  (list (create-ext-exp-open-arc-exp ev wff pos ret))))
	     ; otherwise, building basic edag, with no exp arcs out of exp nodes
	     (set-eeod-exp ret nil)))
	  ((or (and pos (e-bd-wff-p wff))
	       (and (not pos) (a-bd-wff-p wff)))
	   (let* ((x (bindvar wff))
		  (selvar (fresh-var (unabbreviated-type x) (getnameroot x)))
		  (wff2 (substitute-l-term-var selvar x (cdr wff)))
		  (evl (eeod-exp-vars-above ret)))
	     (setf (get selvar 'exp-vars-above) evl)
	     (dolist (ev evl)
	       (push selvar (get ev 'banned-sel-vars)))
	     (let ((arc (create-ext-exp-open-arc-sel selvar wff2 pos ret)))
	       (set-eeod-sel ret (list arc)))))
	  ((equals-p wff)
	   (let* ((lft (cdar wff))
		  (rght (cdr wff))
		  (tp (unabbreviated-type lft)))
	     (if (consp tp)
		 (let ((x (fresh-var-1 (cdr tp)))
		       (q (inherit-abbrev '= (acons 'O (car tp) (car tp)) (list (car tp)))))
		   (set-eeod-rew
		    ret 'EXT=
		    (create-ext-exp-open-arc-1
		     (acons x 'FORALL (acons q (cons lft x) (cons rght x)))
		     pos ret)))
	       (if (eq tp 'O)
		   (set-eeod-rew
		    ret 'EXT=
		    (create-ext-exp-open-arc-1
		     (acons 'EQUIV lft rght) pos ret))
		 (if pos
		     (set-eeod-eqn ret)
		   (let ((arc1 (make-ext-exp-open-arc))
			 (eqngoal (make-ext-exp-open-dag))
			 (eqngoal-name (intern-str (create-namestring 'EQNGOAL))))
		     (set-eeod-eqn ret)
		     (setf (ext-exp-open-dag-arcs ret) (list arc1))
		     (setf (ext-exp-open-arc-parent arc1) ret)
		     (setf (ext-exp-open-arc-kind arc1) 'EQNGOAL)
		     (setf (ext-exp-open-arc-node arc1) eqngoal)
		     (setf (ext-exp-open-dag-parent-arcs eqngoal) (list arc1))
		     (setf (ext-exp-open-dag-kind eqngoal) 'EQNGOAL)
		     (setf (ext-exp-open-dag-name eqngoal) eqngoal-name)
		     (setf (get eqngoal-name 'ext-exp-open-dag) eqngoal)
		     (setf (ext-exp-open-dag-shallow eqngoal) wff)
		     (setf (ext-exp-open-dag-positive eqngoal) nil)
		     (when simple-edag ; if building simple edag, try to decompose - otherwise, don't
		       (push-eqndec-arc wff ret))))))))
	  (t
	   (if (ext-exp-var-p (head wff))
	       (set-eeod-flex ret)
	     (set-eeod-atom ret))))
    ret))

(defun create-ext-exp-open-arc-1 (wff &optional pos parentnode)
  (let ((arc (make-ext-exp-open-arc :parent parentnode)))
    (setf (ext-exp-open-arc-node arc)
	  (create-ext-exp-open-dag-1 wff pos (list arc)))
    arc))


(defun create-ext-exp-open-arc-exp (ev wff &optional pos parentnode)
  (let* ((wff2 (substitute-l-term-var ev (bindvar wff) (cdr wff)))
	 (arc (make-ext-exp-open-arc :parent parentnode :kind 'EXP :exp-term ev)))
    (setf (get ev 'ext-exp-var) t)
    (setf (get ev 'ext-exp-var-arcs) (list arc))
    (setf (ext-exp-open-arc-node arc) (create-ext-exp-open-dag-1 wff2 pos (list arc)))
    arc))

(defun create-ext-exp-open-arc-sel (selvar wff &optional pos parentnode)
  (let ((arc (make-ext-exp-open-arc :parent parentnode :kind 'SEL :sel-var selvar)))
    (setf (ext-exp-open-arc-node arc)
	  (create-ext-exp-open-dag-1 wff pos (list arc)))
    arc))

(defun create-simple-ext-exp-open-arc-decs (n lft rght &optional parentnode)
  (let ((simple-edag t))
    (declare (special simple-edag))
    (create-ext-exp-open-arc-decs n lft rght parentnode)))

(defun create-ext-exp-open-arc-decs (n lft rght &optional parentnode)
  (if (> n 0)
      (let* ((a (cdr lft))
	     (b (cdr rght))
	     (tp (unabbreviated-type a))
	     (q (inherit-abbrev '= (acons 'O tp tp) (list tp)))
	     (wff (acons q a b))
	     (arc (make-ext-exp-open-arc :parent parentnode :kind 'DEC :dec-index (- n 1))))
	(setf (ext-exp-open-arc-node arc)
	      (create-ext-exp-open-dag-2 wff nil (list arc)))
	(cons arc
	      (create-ext-exp-open-arc-decs (- n 1) (car lft) (car rght) parentnode)))
    nil))

(defun create-simple-ext-exp-open-arc-exp-gen (trm wff &optional pos parentnode)
  (let ((simple-edag t))
    (declare (special simple-edag))
    (create-ext-exp-open-arc-exp-gen trm wff pos parentnode)))

(defun create-basic-ext-exp-open-arc-exp-gen (trm wff &optional pos parentnode)
  (let ((simple-edag nil))
    (declare (special simple-edag))
    (create-ext-exp-open-arc-exp-gen trm wff pos parentnode)))

(defun ext-exp-vars-of (trm)
  (remove-if-not #'ext-exp-var-p (free-vars-of trm)))

(defun create-ext-exp-open-arc-exp-gen (trm wff &optional pos parentnode)
  (let* ((wff2 (substitute-l-term-var trm (bindvar wff) (cdr wff)))
	 (arc (make-ext-exp-open-arc :parent parentnode :kind 'EXP :exp-term trm)))
    (dolist (ev (ext-exp-vars-of trm))
      (push arc (get ev 'ext-exp-var-arcs)))
    (setf (ext-exp-open-arc-node arc) (create-ext-exp-open-dag-1 wff2 pos (list arc)))
    arc))
  
(defun eeod-get-nodes (testfn eeod &optional (multiple t))
  (let ((nodes-done nil))
    (declare (special nodes-done))
    (eeod-get-nodes-1 testfn eeod multiple)))

(defun eeod-get-nodes-l (testfn eeodl &optional (multiple t))
  (let ((nodes-done nil)
	(ret nil))
    (declare (special nodes-done))
    (dolist (n eeodl)
      (setq ret (append (eeod-get-nodes-1 testfn n multiple) ret)))
    ret))

(defun eeod-get-nodes-1 (testfn eeod &optional (multiple t))
  (declare (special nodes-done))
  (if (member eeod nodes-done)
      nil
    (progn
      (push eeod nodes-done)
      (if (funcall testfn eeod)
	  (if multiple
	      (cons eeod (eeod-get-nodes-2 testfn (ext-exp-open-dag-arcs eeod) multiple))
	    eeod)
	(eeod-get-nodes-2 testfn (ext-exp-open-dag-arcs eeod) multiple)))))

(defun eeod-get-nodes-2 (testfn arcs &optional (multiple t))
  (if arcs
      (if multiple
	  (append (eeod-get-nodes-1 testfn (ext-exp-open-arc-node (car arcs)) multiple)
		  (eeod-get-nodes-2 testfn (cdr arcs) multiple))
	(or (eeod-get-nodes-1 testfn (ext-exp-open-arc-node (car arcs)) multiple)
	    (eeod-get-nodes-2 testfn (cdr arcs) multiple)))
    nil))

(defun eeod-flex-below-arc-p (arc)
  (eeod-get-nodes #'(lambda (node)
		      (eq (ext-exp-open-dag-kind node) 'FLEX))
		  (ext-exp-open-arc-node arc) nil))

(defun eeod-get-arcs-above (testfn eeod &optional (multiple t))
  (let ((nodes-done nil))
    (declare (special nodes-done))
    (eeod-get-arcs-above-1 testfn eeod multiple)))

(defun eeod-get-arcs-above-l (testfn eeodl &optional (multiple t))
  (let ((nodes-done nil)
	(ret nil))
    (declare (special nodes-done))
    (do ((nl eeodl (cdr nl)))
	((or (null nl) (and (not multiple) ret)))
      (setq ret (union (eeod-get-arcs-above-1 testfn (car nl) multiple) ret)))
    ret))

(defun eeod-get-arcs-above-1 (testfn eeod &optional (multiple t))
  (declare (special nodes-done))
  (unless (member eeod nodes-done)
    (push eeod nodes-done)
    (let ((ret nil))
      (do ((arcs (ext-exp-open-dag-parent-arcs eeod)
		 (cdr arcs)))
	  ((or (null arcs) (and (not multiple) ret)))
	(let ((arc (car arcs)))
	  (when (funcall testfn arc)
	    (push arc ret))
	  (when (or multiple (not ret))
	    (setq ret
		  (append (eeod-get-arcs-above-1
			   testfn (ext-exp-open-arc-parent arc) multiple)
			  ret)))))
      ret)))

(defun eeod-evar-depth-of-node (node)
  (length
   (eeod-get-arcs-above
    #'(lambda (arc)
	(and (eq (ext-exp-open-arc-kind arc) 'EXP)
	     (ext-exp-vars-of (ext-exp-open-arc-exp-term arc))))
    node t)))

(defun eeod-evar-depth (ev)
  (if (ext-exp-var-p ev)
      (apply #'+
	     (mapcar #'(lambda (arc)
			 (eeod-evar-depth-of-node
			  (ext-exp-open-arc-parent arc)))
		     (get ev 'ext-exp-var-arcs)))
    0))

(defun eeod-stamp-order (n1 n2)
  (< (ext-exp-open-dag-stamp n1) (ext-exp-open-dag-stamp n2)))

(defun eeod-exp-arcs-above (node)
  (eeod-get-arcs-above #'(lambda (x)
			   (eq (ext-exp-open-arc-kind x) 'EXP))
		       node))

(defun eeod-exp-arcs-above-l (nodes)
  (eeod-get-arcs-above-l #'(lambda (x)
			   (eq (ext-exp-open-arc-kind x) 'EXP))
			 nodes))

(defun eeod-get-sibling-nodes (testfn eeod)
  (eeod-get-sibling-nodes-1 testfn (ext-exp-open-dag-parent-arcs eeod)))

(defun eeod-get-sibling-nodes-1 (testfn arcs)
  (if arcs
      (append (eeod-get-sibling-nodes-2 testfn (car arcs))
	      (eeod-get-sibling-nodes-1 testfn (cdr arcs)))
    nil))

(defun eeod-get-sibling-nodes-2 (testfn arc)
  (let* ((node (ext-exp-open-arc-parent arc))
	 (nodes1 (eeod-get-sibling-nodes-1
		  testfn (ext-exp-open-dag-parent-arcs node)))
	 (nodes2 (when (funcall testfn node)
		   (mapcar #'(lambda (x)
			       (ext-exp-open-arc-node x))
			   (remove arc (ext-exp-open-dag-arcs node))))))
    (append nodes1 nodes2)))

(defun eeod-find-node (name eeod)
  (let ((nodes (eeod-get-nodes #'(lambda (x) (eq (ext-exp-open-dag-name x) name)) eeod)))
    (if nodes
	(car nodes)
      nil)))

(defun eeod-exp-vars (node)
  (let ((expnodes (eeod-get-nodes #'(lambda (x) (eq (ext-exp-open-dag-kind x) 'EXP)) node))
	(expvars nil))
    (dolist (exp expnodes (remove-duplicates expvars))
      (dolist (arc (ext-exp-open-dag-arcs exp))
	(let ((trm (ext-exp-open-arc-exp-term arc)))
	  (setq expvars (append (ext-exp-vars-of trm)
				expvars)))))))

(defun eeod-exp-vars-above (node)
  (let ((nodes-done nil)
	(evl nil))
    (declare (special nodes-done evl))
    (eeod-exp-vars-above-1 node)
    evl))

(defun eeod-exp-vars-above-1 (node)
  (declare (special nodes-done))
  (unless (member node nodes-done)
    (push node nodes-done)
    (dolist (arc (ext-exp-open-dag-parent-arcs node))
      (eeod-exp-vars-above-2 arc))))

(defun eeod-exp-vars-above-2 (arc)
  (declare (special evl))
  (when (eq (ext-exp-open-arc-kind arc) 'EXP)
    (let* ((trm (ext-exp-open-arc-exp-term arc)))
      (setq evl (append (ext-exp-vars-of trm)
			evl))))
  (eeod-exp-vars-above-1 (ext-exp-open-arc-parent arc)))
				       
(defun eeod-sel-vars (node)
  (let ((selnodes (eeod-get-nodes #'(lambda (x) (eq (ext-exp-open-dag-kind x) 'SEL)) node))
	(selvars nil))
    (dolist (sel selnodes selvars)
      (dolist (arc (ext-exp-open-dag-arcs sel))
	(let ((sv (ext-exp-open-arc-sel-var arc)))
	  (push sv selvars))))))

(defun eeod-sel-var-arcs (node)
  (let ((selnodes (eeod-get-nodes #'(lambda (x) (eq (ext-exp-open-dag-kind x) 'SEL)) node))
	(selvar-arcs nil))
    (dolist (sel selnodes selvar-arcs)
      (dolist (arc (ext-exp-open-dag-arcs sel))
	(let ((sv (ext-exp-open-arc-sel-var arc)))
	  (push (cons sv arc) selvar-arcs))))))

(defun eeod-set-banned-vars (eeod)
  (let ((nodes-selvars nil)
	(selvars-exparcs nil)
	(expvars nil)
	(selvars (eeod-sel-vars eeod)))
    (declare (special nodes-selvars selvars-exparcs expvars selvars))
    (dolist (sv selvars)
      (setf (get sv 'exp-vars-above) nil))
    (eeod-set-banned-vars-1 eeod)
    (dolist (v expvars)
      (setf (get v 'banned-sel-vars)
	    (eeod-set-banned-vars-2 (get v 'ext-exp-var-arcs) nodes-selvars selvars-exparcs)))))

(defun eeod-set-banned-vars-1 (eeod)
  (declare (special nodes-selvars selvars-exparcs expvars selvars))
  (let ((a (assoc eeod nodes-selvars)))
    (if a
	(cdr a)
      (let ((selvars1 nil))
	(dolist (arc (ext-exp-open-dag-arcs eeod))
	  (let ((selvars0 (eeod-set-banned-vars-1 (ext-exp-open-arc-node arc))))
	    (setq selvars1 (union selvars0 selvars1))
	    (when (ext-exp-open-arc-sel-var arc)
	      (let ((sv (ext-exp-open-arc-sel-var arc)))
		(push sv selvars1)))
	    (when (ext-exp-open-arc-exp-term arc)
	      (dolist (fv (free-vars-of (ext-exp-open-arc-exp-term arc)))
		(when (member fv selvars)
		  (let ((z (assoc fv selvars-exparcs)))
		    (if z
			(push arc (cdr z))
		      (push (list fv arc) selvars-exparcs))))
		(when (ext-exp-var-p fv)
		  (unless (member fv expvars)
		    (setf (get fv 'banned-sel-vars) nil)
		    (setf (get fv 'ext-exp-var-arcs) nil)
		    (push fv expvars))
		  (dolist (sv selvars0)
		    (push fv (get sv 'exp-vars-above)))
		  (push arc (get fv 'ext-exp-var-arcs)))))))
	(push (cons eeod selvars1) nodes-selvars)
	selvars1))))

(defun eeod-set-banned-vars-2 (exparcs nodes-selvars selvars-exparcs &optional done)
  (let ((ret nil))
    (dolist (a exparcs ret)
      (unless (member a done)
	(let* ((n (ext-exp-open-arc-node a))
	       (b (assoc n nodes-selvars)))
	  (when b
	    (dolist (s (cdr b))
	      (setq ret (adjoin s ret))
	      (setq ret (union ret
			       (eeod-set-banned-vars-2 (cdr (assoc s selvars-exparcs)) nodes-selvars selvars-exparcs
						       (cons a done)))))))))))

(defun ensure-simple-eeod (eeod)
  (let ((simple-edag t))
    (declare (special simple-edag))
    (let ((nodes
	   (eeod-get-nodes #'(lambda (x)
			       (or (and (eq (ext-exp-open-dag-kind x) 'EXP)
					(not (ext-exp-open-dag-arcs x)))
				   (and (eq (ext-exp-open-dag-kind x) 'EQN)
					(not (ext-exp-open-dag-positive x)))))
			   eeod)))
      (dolist (node nodes)
	(if (eq (ext-exp-open-dag-kind node) 'EXP)
	    (let* ((wff (ext-exp-open-dag-shallow node))
		   (x (bindvar wff))
		   (ev (fresh-var (unabbreviated-type x) (getnameroot x)))
		   (newarc (create-ext-exp-open-arc-exp ev wff (ext-exp-open-dag-positive node) node)))
	      (setf (ext-exp-open-dag-arcs node) (list newarc)))
	  (push-eqndec-arc (ext-exp-open-dag-shallow node) node))))))

 ; shouldn't call this if eeod (the root node) is flexible
(defun eeod-subst-deepen (theta eeod)
  (let ((nodes-done nil)
	(nodes-delayed nil)
	(theta2 nil))
    (declare (special nodes-done nodes-delayed))
    (dolist (p theta)
      (let ((ev (car p))
	    (wff (cdr p)))
	(push (cons ev wff) theta2)
	(setf (get ev 'ext-exp-var-subst) wff)))
    (eeod-subst-deepen-1 theta2 eeod)
    (eeod-set-banned-vars eeod)))

(defun eeod-subst-deepen-1 (theta eeod)
  (declare (special nodes-done nodes-delayed))
  (unless (member eeod nodes-done)
    (push eeod nodes-done)
    (let* ((k (ext-exp-open-dag-kind eeod))
	   (pos (ext-exp-open-dag-positive eeod))
	   (sh (ext-exp-open-dag-shallow eeod))
	   (sh2 (simul-substitute-l-term-var theta sh))
	   (nsh2 (lazy-abbrev-normalize sh2)))
      (setf (ext-exp-open-dag-shallow eeod) nsh2)
      (if (and (eq k 'FLEX) (neq (head sh) (head sh2)))
	  (let ((parent-arcs (ext-exp-open-dag-parent-arcs eeod)))
	    (let ((simple-edag t))
	      (declare (special simple-edag))
	      (let ((node (create-ext-exp-open-dag-2 nsh2 pos parent-arcs)))
		(setf (get (ext-exp-open-dag-name eeod) 'ext-exp-open-dag) node)
		(dolist (arc parent-arcs)
		  (setf (ext-exp-open-arc-node arc) node)))))
	(progn
	  (dolist (arc (ext-exp-open-dag-arcs eeod))
	    (when (ext-exp-open-arc-exp-term arc)
	      (setf (ext-exp-open-arc-exp-term arc)
		    (etanorm
		     (lambda-norm
		      (simul-substitute-l-term-var theta (ext-exp-open-arc-exp-term arc))))))
	    (eeod-subst-deepen-1 theta (ext-exp-open-arc-node arc)))
	  (when (eq k 'EQN)
	    (unless (or pos (find-if #'(lambda (arc)
					 (eq (ext-exp-open-arc-kind arc) 'EQNDEC))
				     (ext-exp-open-dag-arcs eeod)))
	      (push-simple-eqndec-arc nsh2 eeod))))))))

(defun eeod-duplicate-expnode (exp)
  (let* ((wff (ext-exp-open-dag-shallow exp))
	 (pos (ext-exp-open-dag-positive exp))
	 (x (bindvar wff))
	 (ev (fresh-var (unabbreviated-type x) (getnameroot x)))
	 (wff2 (substitute-l-term-var ev x (cdr wff))))
    (let ((arc (make-ext-exp-open-arc :parent exp :kind 'EXP :exp-term ev)))
      (setf (get ev 'ext-exp-var) t)
      (setf (get ev 'ext-exp-var-arcs) (list arc))
      (let ((simple-edag t))
	(declare (special simple-edag))
	(setf (ext-exp-open-arc-node arc)
	      (create-ext-exp-open-dag-2 wff2 pos (list arc))))
      (push arc (ext-exp-open-dag-arcs exp))
      arc)))

(defun eed-to-eeod (node)
  (let ((node-assoc nil))
    (declare (special node-assoc))
    (eed-to-eeod-1 node)))
    
(defun eed-to-eeod-1 (node &optional parent-arc)
  (declare (special node-assoc))
  (let ((a (assoc node node-assoc)))
    (if a
	(let ((ret (cdr a)))
	  (when parent-arc
	    (push parent-arc (ext-exp-open-dag-parent-arcs ret)))
	  ret)
      (if (and (eq (ext-exp-dag-kind node) 'REW)
	       (member (ext-exp-dag-rew-just node) '(LAMBDA EQUIVWFFS)))
	  (eed-to-eeod-1
	   (ext-exp-arc-node (car (ext-exp-dag-arcs node)))
	   parent-arc)
	(if (eq (ext-exp-dag-kind node) 'LEAF)
	    (let ((ret (create-basic-ext-exp-open-dag ; don't make exp arcs or dec arcs (basic edag)
			(ext-exp-dag-shallow node)
			(ext-exp-dag-positive node)
			(when parent-arc
			  (list parent-arc)))))
	      (push (cons node ret) node-assoc)
	      ret)
	  (let ((ret (make-ext-exp-open-dag)))
	    (push (cons node ret) node-assoc)
	    (setf (ext-exp-open-dag-name ret)
		  (ext-exp-dag-name node))
	    (setf (ext-exp-open-dag-shallow ret)
		  (ext-exp-dag-shallow node))
	    (setf (ext-exp-open-dag-positive ret)
		  (ext-exp-dag-positive node))
	    (setf (ext-exp-open-dag-kind ret)
		  (ext-exp-dag-kind node))
	    (setf (ext-exp-open-dag-rew-just ret)
		  (ext-exp-dag-rew-just node))
	    (when parent-arc
	      (setf (ext-exp-open-dag-parent-arcs ret)
		    (list parent-arc)))
	    (setf (ext-exp-open-dag-arcs ret)
		  (mapcar #'(lambda (arc)
			      (eed-to-eeod-2 arc ret))
			  (ext-exp-dag-arcs node)))
	    ret))))))

(defun eed-to-eeod-2 (arc parent)
  (let ((ret (make-ext-exp-open-arc :parent parent)))
    (setf (ext-exp-open-arc-node ret)
	  (eed-to-eeod-1 (ext-exp-arc-node arc) ret))
    (setf (ext-exp-open-arc-kind ret)
	  (ext-exp-arc-kind arc))
    (setf (ext-exp-open-arc-dec-index ret)
	  (ext-exp-arc-dec-index arc))
    (setf (ext-exp-open-arc-exp-term ret)
	  (ext-exp-arc-exp-term arc))
    (setf (ext-exp-open-arc-sel-var ret)
	  (ext-exp-arc-sel-var arc))
    ret))

(defun eeod-to-eed-node (node wff)
  (let ((node-assoc nil))
    (declare (special node-assoc))
    (let ((node2
	   (catch 'unused-node
	     (eeod-to-eed-node-1 node wff))))
      (or node2
	  (make-eed-leaf wff (ext-exp-open-dag-positive node))))))

(defun eeod-to-eed-node-1 (node wff)
  (make-eed-lambda wff (eeod-to-eed-node-2 node wff)))

(defun eeod-to-eed-node-2 (node sh)
  (if (atom-head-abbrev-p sh)
      (make-eed-rew sh 'EQUIVWFFS (eeod-to-eed-node-1 node (instantiate-head-abbrev sh)))
    (eeod-to-eed-node-3 node)))
      
(defun eeod-to-eed-node-3 (node)
  (declare (special node-assoc))
  (let ((a (assoc node node-assoc)))
    (if a
	(if (eq (cdr a) 'UNUSED)
	    (throw 'unused-node nil)
	  (cdr a))
      (let ((k (ext-exp-open-dag-kind node))
	    (pos (ext-exp-open-dag-positive node))
	    (arcs (ext-exp-open-dag-arcs node))
	    (wff (ext-exp-open-dag-shallow node)))
	(case k
	  (TRUE
	   (if pos
	       (throw 'unused-node nil)
	     (make-eed-true nil)))
	  (FALSE
	   (if pos
	       (make-eed-false t)
	     (throw 'unused-node nil)))
	  (NEG
	   (make-eed-neg
	    (eeod-to-eed-node-2 (ext-exp-open-arc-node (car arcs)) (cdr wff))))
	  ((DIS CON IMP)
	   (let ((node21 (catch 'unused-node (eeod-to-eed-node-2 (ext-exp-open-arc-node (car arcs)) (cdar wff))))
		 (node22 (catch 'unused-node (eeod-to-eed-node-2 (ext-exp-open-arc-node (cadr arcs)) (cdr wff)))))
	     (if (or node21 node22)
		 (case k
		   (DIS (make-eed-dis (or node21 (make-eed-leaf (cdar wff) pos))
				      (or node22 (make-eed-leaf (cdr wff) pos))))
		   (CON (make-eed-con (or node21 (make-eed-leaf (cdar wff) pos))
				      (or node22 (make-eed-leaf (cdr wff) pos))))
		   (IMP (make-eed-imp (or node21 (make-eed-leaf (cdar wff) (not pos)))
				      (or node22 (make-eed-leaf (cdr wff) pos)))))
	       (throw 'unused-node nil))))
	  (SEL
	   (let* ((arc (car arcs))
		  (sel-var (ext-exp-open-arc-sel-var arc))
		  (wff2 (substitute-l-term-var sel-var (bindvar wff) (cdr wff))))
	     (make-eed-sel-1 wff sel-var
			     (eeod-to-eed-node-2 (ext-exp-open-arc-node (car arcs)) wff2))))
	  (EXP
	   (let ((arcs2 nil))
	     (dolist (arc arcs)
	       (let ((arc2 (catch 'unused-node (eeod-to-eed-arc-exp wff arc))))
		 (when arc2
		   (push arc2 arcs2))))
	     (if arcs2
		 (make-eea-exp wff pos arcs2)
	       (throw 'unused-node nil))))
	  (REW
	   (case (ext-exp-open-dag-rew-just node)
	     (EXT=
	      (let ((tp (cdr (unabbreviated-type (caar wff)))))
		(if (consp tp)
		    (let* ((lft (cdar wff))
			   (rght (cdr wff))
			   (x (fresh-var-1 (cdr tp)))
			   (q (inherit-abbrev '= (cons (cons 'O (car tp)) (car tp)) (list (car tp))))
			   (wff2 (acons x 'FORALL (acons q (cons lft x) (cons rght x)))))
		      (make-eed-rew wff 'EXT=
				    (eeod-to-eed-node-1 (ext-exp-open-arc-node (car arcs)) wff2)))
		    (make-eed-rew wff 'EXT=
				  (eeod-to-eed-node-3 (ext-exp-open-arc-node (car arcs)))))))
	     (t
	      (make-eed-rew wff (ext-exp-open-dag-rew-just node)
			    (eeod-to-eed-node-3 (ext-exp-open-arc-node (car arcs)))))))
	  (ATOM
	   (let ((arcs2 nil))
	     (dolist (arc arcs)
	       (let* ((kid (ext-exp-open-arc-node arc))
		      (akid (assoc kid node-assoc)))
		 (if akid
		     (unless (eq (cdr akid) 'unused)
		       (push (make-ext-exp-arc :kind 'MATE :node (cdr akid)) arcs2))
		   (let ((kid2 (catch 'unused-node (eeod-to-eed-node-2 kid (ext-exp-open-dag-shallow kid)))))
		     (if kid2
			 (progn
			   (push (cons kid kid2) node-assoc)
			   (push (make-ext-exp-arc :kind 'MATE :node kid2) arcs2))
		       (push (cons kid 'unused) node-assoc))))))
	     (if arcs2
		 (make-ext-exp-dag :name (intern-str (create-namestring 'ATOM))
				   :kind 'ATOM
				   :positive pos
				   :shallow wff
				   :arcs arcs2)
	       (throw 'unused-node nil))))
	  (FLEX (throw 'unused-node nil))
	  (EQN
	   (if pos
	       (let ((arcs2 nil))
		 (dolist (arc arcs)
		   (let* ((kid (ext-exp-open-arc-node arc))
			  (ak (ext-exp-open-arc-kind arc))
			  (akid (assoc kid node-assoc)))
		     (if akid
			 (unless (eq (cdr akid) 'unused)
			   (push (make-ext-exp-arc :kind ak :node (cdr akid)) arcs2))
		       (let ((kid2 (catch 'unused-node (eeod-to-eed-node-3 kid))))
			 (if kid2
			     (progn
			       (push (cons kid kid2) node-assoc)
			       (push (make-ext-exp-arc :kind ak :node kid2) arcs2))
			   (push (cons kid 'unused) node-assoc))))))
		 (if arcs2
		     (make-ext-exp-dag :name (intern-str (create-namestring 'EQN))
				       :kind 'EQN
				       :positive t
				       :shallow wff
				       :arcs arcs2)
		   (throw 'unused-node nil)))
	     (let* ((eqndec-arc (find-if #'(lambda (x) (eq (ext-exp-open-arc-kind x) 'EQNDEC)) arcs))
		    (eqngoal-arc (find-if #'(lambda (x) (eq (ext-exp-open-arc-kind x) 'EQNGOAL)) arcs))
		    (eqndec-node2 (when eqndec-arc
				    (catch 'unused-node (eeod-to-eed-node-3 (ext-exp-open-arc-node eqndec-arc)))))
		    (eqngoal-node2 (when eqngoal-arc
				     (catch 'unused-node (eeod-to-eed-node-3 (ext-exp-open-arc-node eqngoal-arc)))))
		    (arcs2 nil))
	       (when eqndec-node2
		 (push (make-ext-exp-arc :kind 'EQNDEC :node eqndec-node2) arcs2))
	       (when eqngoal-node2
		 (push (make-ext-exp-arc :kind 'EQNGOAL :node eqngoal-node2) arcs2))
	       (if arcs2
		   (make-ext-exp-dag :name (intern-str (create-namestring 'EQN))
				     :kind 'EQN
				     :positive nil
				     :shallow wff
				     :arcs arcs2)
		 (throw 'unused-node nil)))))
	  (DEC
	   (if arcs
	       (let* ((lft (cdar wff))
		      (rght (cdr wff))
		      (lft-args (args lft))
		      (rght-args (args rght))
		      (arcs2 (mapcar #'(lambda (arc)
					 (let* ((i (ext-exp-open-arc-dec-index arc))
						(kid (ext-exp-open-arc-node arc))
						(lfti (nth i lft-args))
						(rghti (nth i rght-args))
						(tpi (unabbreviated-type lfti))
						(qi (inherit-abbrev '= (acons 'O tpi tpi) (list tpi))))
					   (make-ext-exp-arc :kind 'DEC :dec-index i
							     :node (eeod-to-eed-node-2 kid (acons qi lfti rghti)))))
				     arcs)))
		 (make-eed-dec-1 wff arcs2))
	     (make-eed-dec-1 wff nil)))
	  (EQNGOAL ; EUnifs
	   (let ((arcs2 nil))
	     (dolist (arc arcs)
	       (let* ((kid (ext-exp-open-arc-node arc))
		      (ak (ext-exp-open-arc-kind arc))
		      (akid (assoc kid node-assoc)))
		 (if akid
		     (unless (eq (cdr akid) 'unused)
		       (push (make-ext-exp-arc :kind ak :node (cdr akid)) arcs2))
		   (let ((kid2 (catch 'unused-node (eeod-to-eed-node-3 kid))))
		     (if kid2
			 (progn
			   (push (cons kid kid2) node-assoc)
			   (push (make-ext-exp-arc :kind ak :node kid2) arcs2))
		       (push (cons kid 'unused) node-assoc))))))
	     (if arcs2
		 (make-ext-exp-dag :name (intern-str (create-namestring 'EQN))
				   :kind 'EQNGOAL
				   :positive nil
				   :shallow wff
				   :arcs arcs2)
	       (throw 'unused-node nil)))))))))

(defun eeod-to-eed-arc-exp (wff arc)
  (let* ((trm (ext-exp-open-arc-exp-term arc))
	 (wff2 (substitute-l-term-var trm (bindvar wff) (cdr wff))))
    (make-ext-exp-arc :kind 'EXP :exp-term (etanorm (lambda-norm trm))
		      :node
		      (eeod-to-eed-node-1 (ext-exp-open-arc-node arc) wff2))))
  
; checking completeness of eeod's (no vertical paths - without jforms)
; check if there are no "vertical" paths
; (without computing the jform here)
(defun ext-exp-open-dag-complete (eed &optional banned blocked-nodes)
  (let ((open-path nil))
    (declare (special open-path))
    (ext-exp-open-dag-complete-conjs (list eed) banned blocked-nodes)
    (if open-path
	(values nil open-path)
      t)))

(defun ext-exp-open-dag-complete-l (eedl &optional banned blocked-nodes)
  (if eedl
      (let ((open-path nil))
	(declare (special open-path))
	(ext-exp-open-dag-complete-conjs eedl banned blocked-nodes)
	(if open-path
	    (values nil open-path)
	  t))
    (values nil nil)))

(defun ext-exp-open-dag-complete-conjs (eedl &optional banned blocked-nodes)
  (declare (special open-path))
  (let* ((acc (ext-exp-open-dag-accessible-kids eedl banned blocked-nodes))
	 (done (assoc 'DONE acc))
	 (exp (assoc 'EXP acc))
	 (sel (assoc 'SEL acc))
	 (conj (assoc 'CONJ acc))
	 (disj (assoc 'DISJ acc))
	 (eunif (assoc 'EUNIF acc))
	 (mate (assoc 'MATE acc)))
    (cond (done t)
	  (eunif
	   (ext-exp-open-dag-complete-conjs
	    (cons (cadddr eunif) eedl)
	    banned (cons (cadddr eunif) blocked-nodes)))
	  (mate
	   (ext-exp-open-dag-complete-conjs
	    (cons (cadddr mate) eedl)
	    banned (cons (cadddr mate) blocked-nodes)))
	  (sel
	   (let ((sv (ext-exp-open-arc-sel-var (caddr sel)))
		 (selkid (ext-exp-open-arc-node (caddr sel))))
	     (ext-exp-open-dag-complete-conjs
	      (cons selkid eedl)
	      (remove sv banned)
	      (cons selkid blocked-nodes))))
	  (exp
	   (let ((expkid (ext-exp-open-arc-node (caddr exp))))
	     (ext-exp-open-dag-complete-conjs (cons expkid eedl) banned
					 (cons expkid blocked-nodes))))
	  (conj
	   (ext-exp-open-dag-complete-conjs
	    (append (caddr conj) (remove (cadr conj) eedl))
	    banned blocked-nodes))
	  (disj
	   (let ((eedl2 (remove (cadr disj) eedl)))
	     (do ((nl (caddr disj) (cdr nl)))
		 ((or open-path (null nl))
		  (not open-path))
	       (ext-exp-open-dag-complete-conjs (cons (car nl) eedl2) banned blocked-nodes))))
	  (t (setq open-path eedl)))))

(defun ext-exp-open-dag-accessible-kids (eedl banned &optional blocked-nodes)
  (let ((ret nil)
	(mates nil)
	(eunifs nil))
    (do ((eedl2 eedl (cdr eedl2)))
	((null eedl2) ret)
      (let* ((eed (car eedl2))
	     (k (ext-exp-open-dag-kind eed))
	     (pos (ext-exp-open-dag-positive eed))
	     (wff (ext-exp-open-dag-shallow eed)))
	(cond ((or (and (member k '(EQN DEC)) (not pos)
			(or (wffeq-ab (cdar wff) (cdr wff)) ; same
			    (and (ext-exp-var-p (head (cdar wff))) ; or flexflex
				 (ext-exp-var-p (head (cdr wff))))))
		   (and (eq k 'TRUE) (not pos))
		   (and (eq k 'FALSE) pos))
	       (push (list 'DONE eed) ret))
	      ((eq k 'EXP)
	       (dolist (arc (ext-exp-open-dag-arcs eed))
		 (let ((expkid (ext-exp-open-arc-node arc))
		       (exptrm (ext-exp-open-arc-exp-term arc)))
		   (unless (or (member expkid blocked-nodes)
			       (intersection banned (free-vars-of exptrm)))
		     (push (list 'EXP eed arc) ret)))))
	      ((eq k 'SEL)
	       (dolist (arc (ext-exp-open-dag-arcs eed))
		 (let ((selkid (ext-exp-open-arc-node arc)))
		   (unless (member selkid blocked-nodes)
		     (push (list 'SEL eed arc) ret)))))
	      ((or (member k '(REW NEG))
		   (and (eq k 'CON) pos)
		   (and (member k '(DIS IMP EQN)) (not pos)))
	       (push (list 'CONJ eed (ext-exp-open-dag-kids eed))
		     ret))
	      ((or (and (eq k 'CON) (not pos))
		   (and (member k '(DIS IMP)) pos)
		   (eq k 'DEC))
	       (push (list 'DISJ eed (ext-exp-open-dag-kids eed))
		     ret))
	      ((member k '(EQN EQNGOAL))
	       (dolist (arc (ext-exp-open-dag-arcs eed))
		 (let* ((n (ext-exp-open-arc-node arc))
			(a (assoc n eunifs)))
		   (if (and a (not (member n blocked-nodes)))
		       (if (ext-exp-open-dag-positive eed)
			   (push (list 'EUNIF (cdr a) eed n (ext-exp-open-arc-kind arc)) ret)
			 (push (list 'EUNIF eed (cdr a) n (ext-exp-open-arc-kind arc)) ret))
		     (push (cons n eed) eunifs)))))
	      ((eq k 'ATOM)
	       (dolist (arc (ext-exp-open-dag-arcs eed))
		 (let* ((n (ext-exp-open-arc-node arc))
			(a (assoc n mates)))
		   (if (and a (not (member n blocked-nodes)))
		       (if (ext-exp-open-dag-positive eed)
			   (push (list 'MATE (cdr a) eed n) ret)
			 (push (list 'MATE eed (cdr a) n) ret))
		     (push (cons n eed) mates)))))
	      (t nil))))))

(defun ext-exp-open-dag-deep (eeod)
  (case (ext-exp-open-dag-kind eeod)
    (TRUE 'TRUTH)
    (FALSE 'FALSEHOOD)
    (NEG (cons 'NOT (ext-exp-open-dag-deep
		     (ext-exp-open-arc-node
		      (car (ext-exp-open-dag-arcs eeod))))))
    ((DIS CON IMP)
     (let ((arcs (ext-exp-open-dag-arcs eeod)))
       (acons (case (ext-exp-open-dag-kind eeod)
		(DIS 'OR)
		(CON 'AND)
		(IMP 'IMPLIES))
	      (ext-exp-open-dag-deep
	       (ext-exp-open-arc-node (car arcs)))
	      (ext-exp-open-dag-deep
	       (ext-exp-open-arc-node (cadr arcs))))))
    ((SEL REW)
     (ext-exp-open-dag-deep
      (ext-exp-open-arc-node
       (car (ext-exp-open-dag-arcs eeod)))))
    (EXP
     (let ((arcs (ext-exp-open-dag-arcs eeod)))
       (if arcs
	   (let ((deep (ext-exp-open-dag-deep
			(ext-exp-open-arc-node (car arcs))))
		 (conn (if (ext-exp-open-dag-positive eeod) 'AND 'OR)))
	     (dolist (arc (cdr arcs))
	       (setq deep
		     (acons conn
			    deep
			    (ext-exp-open-dag-deep
			     (ext-exp-open-arc-node arc)))))
	     deep)
	 (ext-exp-open-dag-shallow eeod))))
    (t
     (ext-exp-open-dag-shallow eeod))))

(defun ftree-to-edag (f clist lemmas)
  (ftree-to-edag-1 f clist lemmas)
  (eeod-set-banned-vars *current-edag*)
  (setq *current-edag-lemmas* lemmas))

(defun ftree-to-edag-1 (f clist lemmas)
  (let ((subst nil)
	(node-assoc nil)
	(selvars nil))
    (declare (special subst node-assoc selvars))
    (if lemmas
	(progn
	  (ftree-to-edag-2 (cadr (ftree-components f)) clist)
	  (let ((lf (car (ftree-components f))))
	    (setq *current-edag-lemma-ftree-pfs* (list lf clist))))
      (ftree-to-edag-2 f clist))))

(defun ftree-to-edag-2 (f clist &optional parent-arc)
  (declare (special *current-edag* selvars))
  (setq *current-edag* (ftree-to-edag-3 f parent-arc))
  (dolist (conn clist)
    (eeod-generalized-connection (car conn) (cdr conn))))

(defun ftree-to-edag-3 (f &optional parent-arc)
  (declare (special subst node-assoc selvars))
  (let* ((sh (lazy-abbrev-normalize (ftree-shallow f)))
	 (e (unless (eq (ftree-kind f) 'LEAF)
	      (make-ext-exp-open-dag :parent-arcs (if parent-arc (list parent-arc) nil)
				     :shallow sh
				     :positive (ftree-positive f)
				     :name (ftree-name f)))))
    (case (ftree-kind f)
      ((TRUE FALSE NEG DIS CON IMP)
       (setf (ext-exp-open-dag-kind e) (ftree-kind f))
       (setf (ext-exp-open-dag-arcs e)
	     (mapcar #'(lambda (x)
			 (let ((arc (make-ext-exp-open-arc :parent e)))
			   (setf (ext-exp-open-arc-node arc)
				 (ftree-to-edag-3 x arc))
			   arc))
		     (ftree-components f))))
      (REW
       (case (ftree-rew-just f)
	 ((EQUIV-IMPLICS EQUIV-DISJS EXT=)
	  (setf (ext-exp-open-dag-kind e) 'REW)
	  (setf (ext-exp-open-dag-rew-just e) (ftree-rew-just f))
	  (setf (ext-exp-open-dag-arcs e)
		(list (let ((arc (make-ext-exp-open-arc :parent e)))
			(setf (ext-exp-open-arc-node arc)
			      (ftree-to-edag-3 (car (ftree-components f)) arc))
			arc))))
;	 ((EQUIVWFFS DEFN BINDER-DEFN)
;	  (setf (ext-exp-open-dag-kind e) 'REW)
;	  (setf (ext-exp-open-dag-rew-just e) 'EQUIVWFFS)
;	  (setf (ext-exp-open-dag-arcs e)
;		(list (let ((arc (make-ext-exp-open-arc :parent e)))
;			(setf (ext-exp-open-arc-node arc)
;			      (ftree-to-edag-3 (car (ftree-components f)) arc))
;			arc))))
	 ((BETA LAMBDA ETA EQUIVWFFS DEFN BINDER-DEFN)
	  (setq e (ftree-to-edag-3 (car (ftree-components f)) parent-arc)))
	 (t
	  (setq e (create-basic-ext-exp-open-dag sh (ftree-positive f))) ; just put in a basic edag (no exp arcs or dec arcs)
	  (setf (ext-exp-open-dag-parent-arcs e) (list parent-arc)))))
      (EXP
       (setf (ext-exp-open-dag-kind e) 'EXP)
       (let* ((eterms (ftree-exp-terms f))
	      (esubs (ftree-exp-subs f))
	      (evs nil))
	 (dolist (pair esubs)
	   (unless (assoc (car pair) subst)
	     (push (cons (car pair) (simul-substitute-l-term-var subst (cdr pair))) subst))
	   (when (eq (car pair) (cdr pair))
	     (unless (member (car pair) selvars)
	       (setf (get (car pair) 'ext-exp-var) t)
	       (push (car pair) evs))))
	 (setq eterms
	       (mapcar #'(lambda (eterm)
			   (simul-substitute-l-term-var subst eterm))
		       eterms))
	 (setf (ext-exp-open-dag-arcs e)
	       (mapcar #'(lambda (y x)
			   (let ((arc (make-ext-exp-open-arc :parent e :kind 'EXP :exp-term y)))
			     (setf (ext-exp-open-arc-node arc)
				   (ftree-to-edag-3 x arc))
			     (dolist (ev evs)
			       (push arc (get ev 'ext-exp-var-arcs)))
			     arc))
		       eterms
		       (ftree-components f)))))
      (SEL
       (setf (ext-exp-open-dag-kind e) 'SEL)
       (let ((sv (ftree-sel-var f)))
	 (push sv selvars)
	 (setf (get sv 'ext-exp-var-p) nil)
	 (setf (get sv 'ext-exp-var-arcs) nil)
	 (setf (ext-exp-open-dag-arcs e)
	       (list (let ((arc (make-ext-exp-open-arc :parent e :kind 'SEL :sel-var sv)))
		       (setf (ext-exp-open-arc-node arc)
			     (ftree-to-edag-3 (car (ftree-components f)) arc))
		       arc)))))
      (LEAF
       (setq e (create-basic-ext-exp-open-dag sh (ftree-positive f)))
       (setf (ext-exp-open-dag-parent-arcs e) (list parent-arc)))
      (t (throwfail "Trouble translating ftree to edag")))
    (push (cons (ftree-name f) e) node-assoc)
    e))

(defun eeod-generalized-connection (conn1 conn2)
  (declare (special node-assoc))
  (let ((n1 (cdr (assoc conn1 node-assoc)))
	(n2 (cdr (assoc conn2 node-assoc))))
    (unless n1
      (setq n1 (get conn1 'ext-exp-open-dag)))
    (unless n2
      (setq n2 (get conn2 'ext-exp-open-dag)))
    (when (and n1 n2 (ext-exp-open-dag-p n1) (ext-exp-open-dag-p n2))
      (eeod-generalized-connection-1 n1 n2))))

(defun eeod-generalized-connection-1 (n1 n2)
  (cond ((and (eq (ext-exp-open-dag-kind n1) 'EQN)
	      (not (ext-exp-open-dag-positive n1)))
	 (let ((arc (find-if #'(lambda (arc)
				 (eq (ext-exp-open-arc-kind arc) 'EQNGOAL))
			     (ext-exp-open-dag-arcs n1))))
	   (when arc
	     (eeod-generalized-connection-1 (ext-exp-open-arc-node arc) n2))))
	((and (eq (ext-exp-open-dag-kind n2) 'EQN)
	      (not (ext-exp-open-dag-positive n2)))
	 (let ((arc (find-if #'(lambda (arc)
				 (eq (ext-exp-open-arc-kind arc) 'EQNGOAL))
			     (ext-exp-open-dag-arcs n1))))
	   (when arc
	     (eeod-generalized-connection-1 n1 (ext-exp-open-arc-node arc)))))
	((and (eq (ext-exp-open-dag-kind n1) 'EQN)
	      (eq (ext-exp-open-dag-kind n2) 'EQNGOAL))
	 (let* ((eunif (eeod-eunif1 n1 n2))
		(conjs (ext-exp-open-dag-kids eunif)))
	   (eeod-generalized-refl-1 (car conjs))
	   (eeod-generalized-refl-1 (cadr conjs))))
	((and (eq (ext-exp-open-dag-kind n1) 'EQNGOAL)
	      (eq (ext-exp-open-dag-kind n2) 'EQN))
	 (let* ((eunif (eeod-eunif1 n2 n1))
		(conjs (ext-exp-open-dag-kids eunif)))
	   (eeod-generalized-refl-1 (car conjs))
	   (eeod-generalized-refl-1 (cadr conjs))))
	((and (eq (ext-exp-open-dag-kind n1) 'EXP)
	      (eq (ext-exp-open-dag-kind n2) 'SEL))
	 (let* ((sarc (car (ext-exp-open-dag-arcs n2)))
		(sv (ext-exp-open-arc-sel-var sarc))
		(earc (car (ext-exp-open-dag-arcs n2)))
		(ev (when earc
		      (ext-exp-open-arc-exp-term earc))))
	   (setf (get sv 'ext-exp-var-p) nil)
	   (setf (get sv 'ext-exp-var-arcs) nil)
	   (when (and earc ev (ext-exp-var-p ev))
	     (eeod-subst-deepen (acons ev sv nil) *current-edag*)
	     (eeod-generalized-connection-1
	      (ext-exp-open-arc-node (car (ext-exp-open-dag-arcs n1)))
	      (ext-exp-open-arc-node (car (ext-exp-open-dag-arcs n2)))))))
	((and (eq (ext-exp-open-dag-kind n1) 'SEL)
	      (eq (ext-exp-open-dag-kind n2) 'EXP))
	 (let* ((sarc (car (ext-exp-open-dag-arcs n2)))
		(sv (ext-exp-open-arc-sel-var sarc))
		(earc (car (ext-exp-open-dag-arcs n1)))
		(ev (when earc
		      (ext-exp-open-arc-exp-term earc))))
	   (setf (get sv 'ext-exp-var-p) nil)
	   (setf (get sv 'ext-exp-var-arcs) nil)
	   (when (and earc ev (ext-exp-var-p ev))
	     (eeod-subst-deepen (acons ev sv nil) *current-edag*)
	     (eeod-generalized-connection-1
	      (ext-exp-open-arc-node (car (ext-exp-open-dag-arcs n1)))
	      (ext-exp-open-arc-node (car (ext-exp-open-dag-arcs n2)))))))
	((and (eq (ext-exp-open-dag-kind n1) (ext-exp-open-dag-kind n2))
	      (not (equal (ext-exp-open-dag-positive n1)
			  (ext-exp-open-dag-positive n2))))
	 (case (ext-exp-open-dag-kind n1)
	   (ATOM
	    (let ((mate (if (ext-exp-open-dag-positive n1)
			    (eeod-mate n1 n2)
			  (eeod-mate n2 n1))))
	      (eeod-generalized-refl-1 mate)))
	   (REW
	    (when (eq (ext-exp-open-dag-rew-just n1) (ext-exp-open-dag-rew-just n2))
	      (eeod-generalized-connection-1
	       (ext-exp-open-arc-node (car (ext-exp-open-dag-arcs n1)))
	       (ext-exp-open-arc-node (car (ext-exp-open-dag-arcs n2))))))
	   (NEG
	    (eeod-generalized-connection-1
	     (ext-exp-open-arc-node (car (ext-exp-open-dag-arcs n1)))
	     (ext-exp-open-arc-node (car (ext-exp-open-dag-arcs n2)))))
	   ((CON DIS IMP)
	    (eeod-generalized-connection-1
	     (ext-exp-open-arc-node (car (ext-exp-open-dag-arcs n1)))
	     (ext-exp-open-arc-node (car (ext-exp-open-dag-arcs n2))))
	    (eeod-generalized-connection-1
	     (ext-exp-open-arc-node (cadr (ext-exp-open-dag-arcs n1)))
	     (ext-exp-open-arc-node (cadr (ext-exp-open-dag-arcs n2)))))
	   (t nil)))
	(t nil)))

; negeqn is a negative node with shallow = or equiv
(defun eeod-generalized-refl-1 (negeqn)
  (case (ext-exp-open-dag-kind negeqn)
    (DEC
     (dolist (arc (ext-exp-open-dag-arcs negeqn))
       (eeod-generalized-refl-1 (ext-exp-open-arc-node arc))))
    (EQN
     (let ((arc (find-if #'(lambda (arc)
			     (eq (ext-exp-open-arc-kind arc) 'EQNDEC))
			 (ext-exp-open-dag-arcs negeqn))))
       (when arc
	 (eeod-generalized-refl-1 (ext-exp-open-arc-node arc)))))
    (REW
     (case (ext-exp-open-dag-rew-just negeqn)
       (EXT=
	(let ((n (ext-exp-open-arc-node (car (ext-exp-open-dag-arcs negeqn)))))
	  (if (eq (ext-exp-open-dag-kind n) 'REW)
	      (eeod-generalized-refl-1 n)
	    (when (eq (ext-exp-open-dag-kind n) 'SEL)
	      (eeod-generalized-refl-1 (ext-exp-open-arc-node (car (ext-exp-open-dag-arcs n))))))))
       (EQUIV-IMPLICS
	(let ((n (ext-exp-open-arc-node (car (ext-exp-open-dag-arcs negeqn)))))
	  (when (eq (ext-exp-open-dag-kind n) 'CON)
	    (let ((imp1 (ext-exp-open-arc-node (car (ext-exp-open-dag-arcs n))))
		  (imp2 (ext-exp-open-arc-node (cadr (ext-exp-open-dag-arcs n)))))
	      (when (eq (ext-exp-open-dag-kind imp1) 'IMP)
		(let ((n1 (ext-exp-open-arc-node (car (ext-exp-open-dag-arcs imp1))))
		      (n2 (ext-exp-open-arc-node (cadr (ext-exp-open-dag-arcs imp1)))))
		  (eeod-generalized-connection-1 n1 n2)))
	      (when (eq (ext-exp-open-dag-kind imp2) 'IMP)
		(let ((n1 (ext-exp-open-arc-node (car (ext-exp-open-dag-arcs imp2))))
		      (n2 (ext-exp-open-arc-node (cadr (ext-exp-open-dag-arcs imp2)))))
		  (eeod-generalized-connection-1 n1 n2)))))))
       (EQUIV-DISJS
	(let ((n (ext-exp-open-arc-node (car (ext-exp-open-dag-arcs negeqn)))))
	  (when (eq (ext-exp-open-dag-kind n) 'DIS)
	    (let ((con1 (ext-exp-open-arc-node (car (ext-exp-open-dag-arcs n))))
		  (con2 (ext-exp-open-arc-node (cadr (ext-exp-open-dag-arcs n)))))
	      (when (and (eq (ext-exp-open-dag-kind con1) 'CON)
			 (eq (ext-exp-open-dag-kind con2) 'CON))
		(let ((n1 (ext-exp-open-arc-node (car (ext-exp-open-dag-arcs con1))))
		      (n2 (ext-exp-open-arc-node (cadr (ext-exp-open-dag-arcs con1))))
		      (n3 (ext-exp-open-arc-node (car (ext-exp-open-dag-arcs con2))))
		      (n4 (ext-exp-open-arc-node (cadr (ext-exp-open-dag-arcs con2)))))
		  (when (eq (ext-exp-open-dag-kind n3) 'NEG)
		    (let ((n5 (ext-exp-open-arc-node (car (ext-exp-open-dag-arcs n3)))))
		      (eeod-generalized-connection-1 n1 n5)
		      (eeod-generalized-connection-1 n2 n5)))
		  (when (eq (ext-exp-open-dag-kind n4) 'NEG)
		    (let ((n6 (ext-exp-open-arc-node (car (ext-exp-open-dag-arcs n4)))))
		      (eeod-generalized-connection-1 n1 n6)
		      (eeod-generalized-connection-1 n2 n6)))))))))
       (t nil)))
    (t nil)))

(defun ext-exp-open-dag-kids (node &optional k)
  (mapcar #'(lambda (arc) (ext-exp-open-arc-node arc))
	  (if k
	      (remove-if-not #'(lambda (arc)
				 (eq (ext-exp-open-arc-kind arc) k))
			     (ext-exp-open-dag-arcs node))
	    (ext-exp-open-dag-arcs node))))

(defun eeod-var-exparc-assoc (node v1 v2)
  (let ((nodes-done nil)
	(arc-assoc nil))
    (declare (special nodes-done arc-assoc))
    (eeod-var-exparc-assoc-1 node v1 v2)
    arc-assoc))

(defun eeod-var-exparc-assoc-1 (node v1 v2)
  (declare (special nodes-done arc-assoc))
  (unless (member node nodes-done)
    (push node nodes-done)
    (let ((arc1 nil)
	  (arc2 nil))
      (when (eq (ext-exp-open-dag-kind node) 'EXP)
	(setq arc1 (find-if #'(lambda (arc)
				(free-in v1 (ext-exp-open-arc-exp-term arc)))
			    (ext-exp-open-dag-arcs node)))
	(when arc1
	  (let ((trm (substitute-l-term-var v2 v1 (ext-exp-open-arc-exp-term arc1))))
	    (setq arc2 (find-if #'(lambda (arc)
				    (wffeq-ab trm (ext-exp-open-arc-exp-term arc)))
				(ext-exp-open-dag-arcs node))))))
      (when (and arc1 arc2)
	(push (cons arc1 arc2) arc-assoc))
      (dolist (kid (ext-exp-open-dag-kids node))
	(eeod-var-exparc-assoc-1 kid v1 v2)))))

(defun eeod-node-assoc-1 (node1 node2)
  (declare (special node-assoc))
  (unless (assoc node1 node-assoc)
    (push (cons node1 node2) node-assoc)
    (let ((kids1 (ext-exp-open-dag-kids node1))
	  (kids2 (ext-exp-open-dag-kids node2)))
      (when (and (eq (ext-exp-open-dag-kind node1) (ext-exp-open-dag-kind node2))
		 (= (length kids1) (length kids2)))
	(mapc #'(lambda (x y)
		  (eeod-node-assoc-1 x y))
	      kids1 kids2)))))

(defun ftree-clear-eeod-names (f)
  (setf (get (ftree-name f) 'ext-exp-open-dag) nil)
  (dolist (c (ftree-components f))
    (ftree-clear-eeod-names c)))

; for debugging
(defun ext-exp-open-dag-check-structure-l (eedl)
  (let ((nodes-delayed nil)
	(nodes-checked nil))
    (declare (special nodes-delayed nodes-checked))
    (ext-exp-open-dag-check-structure-l-1
     (mapcar #'(lambda (x)
		 (list x (ext-exp-open-dag-parent-arcs x)))
	     eedl))))

(defun ext-exp-open-dag-check-structure-l-1 (eedl)
  (if eedl
      (and (ext-exp-open-dag-check-structure-1 (caar eedl)
					       (ext-exp-open-dag-positive (caar eedl))
					       (cadar eedl))
	   (ext-exp-open-dag-check-structure-l-1 (cdr eedl)))
    t))

(defun ext-exp-open-dag-check-structure-1 (eed pos1 par-arcs)
  (ext-exp-open-dag-check-structure-2 eed pos1 par-arcs (ext-exp-open-dag-shallow eed)))

(defun ext-exp-open-dag-check-structure-2 (eed pos1 par-arcs sh1)
  (declare (special nodes-delayed nodes-checked))
  (if (member eed nodes-checked)
      t
    (let ((sh (ext-exp-open-dag-shallow eed))
	  (pos (ext-exp-open-dag-positive eed))
	  (k (ext-exp-open-dag-kind eed))
	  (arcs (ext-exp-open-dag-arcs eed)))
      (when (or (not (equal pos1 pos)) (not (wffeq-ab sh (lazy-abbrev-normalize sh1))))
	(setf (get 'ext-exp-open-dag-debug 'eeod) eed)
	(setf (get 'ext-exp-open-dag-debug 'sh) sh1)
	(setf (get 'ext-exp-open-dag-debug 'pos) pos1)
	(throwfail "structure error in " eed))
      (when (not (and (subsetp par-arcs (ext-exp-open-dag-parent-arcs eed))
		      (subsetp (ext-exp-open-dag-parent-arcs eed) par-arcs)))
	(setf (get 'ext-exp-open-dag-debug 'eeod) eed)
	(setf (get 'ext-exp-open-dag-debug 'par-arcs) par-arcs)
	(throwfail "parent arcs error in " eed))
      (push eed nodes-checked)
      (case k
	(TRUE (and (wffeq-ab sh 'TRUTH) (not arcs)))
	(FALSE (and (wffeq-ab sh 'FALSEHOOD) (not arcs)))
	(NEG (and (not-p sh) (= (length arcs) 1)
		  (ext-exp-open-dag-check-structure-2
		   (ext-exp-open-arc-node (car arcs)) (not pos) (list (car arcs)) (cdr sh))))
	(DIS (and (or-p sh) (= (length arcs) 2)
		  (ext-exp-open-dag-check-structure-2
		   (ext-exp-open-arc-node (car arcs)) pos (list (car arcs)) (cdar sh))
		  (ext-exp-open-dag-check-structure-2
		   (ext-exp-open-arc-node (cadr arcs)) pos (list (cadr arcs)) (cdr sh))))
	(CON (and (and-p sh) (= (length arcs) 2)
		  (ext-exp-open-dag-check-structure-2
		   (ext-exp-open-arc-node (car arcs)) pos (list (car arcs)) (cdar sh))
		  (ext-exp-open-dag-check-structure-2
		   (ext-exp-open-arc-node (cadr arcs)) pos (list (cadr arcs)) (cdr sh))))
	(IMP (and (implies-p sh) (= (length arcs) 2)
		  (ext-exp-open-dag-check-structure-2
		   (ext-exp-open-arc-node (car arcs)) (not pos) (list (car arcs)) (cdar sh))
		  (ext-exp-open-dag-check-structure-2
		   (ext-exp-open-arc-node (cadr arcs)) pos (list (cadr arcs)) (cdr sh))))
	(EXP (and (ae-bd-wff-p sh)
		  (ext-exp-open-arc-check-structure-exp-1 pos (bindvar sh) (cdr sh) arcs)))
	(SEL (and (ae-bd-wff-p sh)
		  (ext-exp-open-arc-check-structure-sel-1 pos (bindvar sh) (cdr sh) arcs)))
	(REW (and (= (length arcs) 1)
		  (ext-exp-open-dag-check-structure-1 (ext-exp-open-arc-node (car arcs)) pos (list (car arcs)))))
	(ATOM
	 (let ((ret t))
	   (dolist (arc arcs ret)
	     (let* ((node (ext-exp-open-arc-node arc))
		    (na (assoc node nodes-delayed)))
	       (if na
		   (let ((pos2 (cadr na))
			 (sh2 (caddr na))
			 (arc2 (cadddr na)))
		     (setq ret (and ret (not (equal pos pos2))
				    (ext-exp-open-dag-check-structure-2
				     node nil (list arc arc2)
				     (if pos
					 (acons (inherit-abbrev '= '((O . O) . O) '(O))
						sh2 sh)
				       (acons (inherit-abbrev '= '((O . O) . O) '(O))
					      sh sh2))))))
		 (push (list node pos sh arc) nodes-delayed))))))
	(EQN
	 (and (equals-p sh)
	      (if pos
		  (let ((ret t)
			(lft1 (cdar sh))
			(rght1 (cdr sh)))
		    (dolist (arc arcs ret)
		      (let* ((node (ext-exp-open-arc-node arc))
			     (na (assoc node nodes-delayed)))
			(if na
			    (let* ((lft2 (cadr na))
				   (rght2 (caddr na))
				   (pos2 (nth 3 na))
				   (arc2 (nth 5 na))
				   (tp (unabbreviated-type lft1))
				   (q (inherit-abbrev '= (cons (cons 'O tp) tp) (list tp)))
				   (arckind2 (nth 4 na)))
			      (setq ret (and ret (not pos2) (equal arckind2 (ext-exp-open-arc-kind arc))
					     (equal tp (unabbreviated-type lft2))
					     (ext-exp-open-dag-check-structure-2
					      node nil (list arc arc2)
					      (if (eq arckind2 'EUNIF1)
						  (acons 'AND
							 (acons q lft1 lft2)
							 (acons q rght1 rght2))
						(acons 'AND
						       (acons q lft1 rght2)
						       (acons q rght1 lft2)))))))
			  (push (list node lft1 rght1 t (ext-exp-open-arc-kind arc) arc) nodes-delayed)))))
		(and (<= (length arcs) 2)
		     (or (not (car arcs)) (ext-exp-open-dag-check-structure-2 (ext-exp-open-arc-node (car arcs)) pos
									      (list (car arcs)) sh))
		     (or (not (cadr arcs)) (ext-exp-open-dag-check-structure-2 (ext-exp-open-arc-node (cadr arcs)) pos
									       (list (cadr arcs)) sh))))))
	(DEC
	 (let* ((ret t)
		(lft (cdar sh))
		(rght (cdr sh))
		(args1 (args lft))
		(args2 (args rght))
		(n1 (length args1))
		(n2 (length args2)))
	   (and (= n1 n2) (not pos)
		(= n1 (length arcs))
		(dotimes (i n1 ret)
		  (let* ((arc (find-if #'(lambda (x) (equal i (ext-exp-open-arc-dec-index x))) arcs))
			 (arg1 (nth i args1))
			 (arg2 (nth i args2))
			 (tp1 (unabbreviated-type arg1))
			 (tp2 (unabbreviated-type arg2)))
		    (setq ret (and ret (equal tp1 tp2)
				   (ext-exp-open-dag-check-structure-2
				    (ext-exp-open-arc-node arc) nil (list arc)
				    (acons (inherit-abbrev '= (cons (cons 'O tp1) tp1) (list tp1))
					   arg1 arg2)))))))))
	(EQNGOAL
	 (and (equals-p sh) (not pos)
	      (let ((ret t)
		    (lft2 (cdar sh))
		    (rght2 (cdr sh)))
		(dolist (arc arcs ret)
		  (let* ((node (ext-exp-open-arc-node arc))
			 (na (assoc node nodes-delayed)))
		    (if na
			(let* ((lft1 (cadr na))
			       (rght1 (caddr na))
			       (pos1 (nth 3 na))
			       (arc2 (nth 5 na))
			       (tp (unabbreviated-type lft1))
			       (q (inherit-abbrev '= (cons (cons 'O tp) tp) (list tp)))
			       (arckind1 (nth 4 na)))
			  (setq ret (and ret pos1 (equal arckind1 (ext-exp-open-arc-kind arc))
					 (equal tp (unabbreviated-type lft2))
					 (ext-exp-open-dag-check-structure-2
					  node nil (list arc arc2)
					  (if (eq arckind1 'EUNIF1)
					      (acons 'AND
						     (acons q lft1 lft2)
						     (acons q rght1 rght2))
					    (acons 'AND
						   (acons q lft1 rght2)
						   (acons q rght1 lft2)))))))
		      (push (list node lft2 rght2 nil (ext-exp-open-arc-kind arc) arc) nodes-delayed)))))))
	((FLEX LEAF) t)
	(t (throwfail "Unknown node " eed))))))

(defun ext-exp-open-arc-check-structure-exp-1 (pos1 x sh arcs)
  (if arcs
      (let* ((exptrm (ext-exp-open-arc-exp-term (car arcs)))
	     (sh2 (lazy-abbrev-normalize (substitute-l-term-var exptrm x sh))))
	(and (ext-exp-open-dag-check-structure-2 (ext-exp-open-arc-node (car arcs)) pos1 (list (car arcs)) sh2)
	     (ext-exp-open-arc-check-structure-exp-1 pos1 x sh (cdr arcs))))
    t))

(defun ext-exp-open-arc-check-structure-sel-1 (pos1 x sh arcs)
  (if arcs
      (let* ((sv (ext-exp-open-arc-sel-var (car arcs)))
	     (sh2 (substitute-l-term-var sv x sh)))
	(and (ext-exp-open-dag-check-structure-2 (ext-exp-open-arc-node (car arcs)) pos1 (list (car arcs)) sh2)
	     (ext-exp-open-arc-check-structure-sel-1 pos1 x sh (cdr arcs))))
    t))

