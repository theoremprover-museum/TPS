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
;;; File: EXT-EXP-DAGS  - cebrown - 3/03

(deffile ext-exp-dags
    (part-of EXT-DAGS)
  (extension clisp)
  (mhelp "Extensional Expansion Dags"))

(context ext-exp-dags)

(defun print-ext-exp-dag (d s k)
  (let ((*standard-output* (make-string-output-stream)))
    (if ext-exp-dag-verbose
	(print-ext-exp-dag-verbose d)
      (format t "~d " (ext-exp-dag-name d)))
    (let ((str (get-output-stream-string *standard-output*)))
      (write-string str s)
      (close *standard-output*))))

(defun print-ext-exp-arc (d s k)
  (let ((*standard-output* (make-string-output-stream)))
    (if ext-exp-dag-verbose
	(print-ext-exp-arc-verbose d)
      (format t "~d " (or (ext-exp-arc-kind d) "Arc")))
    (let ((str (get-output-stream-string *standard-output*)))
      (write-string str s)
      (close *standard-output*))))

(defun print-ext-exp-arc-verbose (d)
  (let ((printed-nodes nil))
    (declare (special printed-nodes))
    (print-ext-exp-arc-verbose-rec d)))

(defun print-ext-exp-dag-verbose (d)
  (let ((printed-nodes nil))
    (declare (special printed-nodes))
    (print-ext-exp-dag-verbose-rec d)))

(defun print-ext-exp-arc-verbose-rec (d)
  (let ((n (ext-exp-arc-node d)))
    (if (ext-exp-arc-exp-term d)
	(progn
	  (format t "~%  EXP: ")
	  (pwff (ext-exp-arc-exp-term d))
	  (format t " --> ~d~%" (ext-exp-dag-name n)))
      (if (ext-exp-arc-sel-var d)
	  (progn
	    (format t "~%  SEL: ")
	    (pwff (ext-exp-arc-sel-var d))
	    (format t "  --> ~d~%" (ext-exp-dag-name n)))
	(if (eq (ext-exp-arc-kind d) 'DEC)
	    (format t "~%   -DEC~d-> ~d" (ext-exp-arc-dec-index d) (ext-exp-dag-name n))
	  (if (ext-exp-arc-kind d)
	      (format t "~%   -~d-> ~d" (ext-exp-arc-kind d) (ext-exp-dag-name n))
	    (format t "~%   --> ~d" (ext-exp-dag-name n))))))
    (print-ext-exp-dag-verbose-rec n)))

(defun print-ext-exp-dag-verbose-rec (d)
  (declare (special printed-nodes))
  (unless (member d printed-nodes)
    (push d printed-nodes)
    (format t "~%~d ~d:~d~%" 
	    (ext-exp-dag-name d)
	    (if (ext-exp-dag-positive d) "+" "-")
	    (or (ext-exp-dag-rew-just d)
		(ext-exp-dag-kind d)))
    (pwff (ext-exp-dag-shallow d))
    (when (ext-exp-dag-arcs d)
      (format t "~%   ->")
      (dolist (z (ext-exp-dag-arcs d))
	(if (ext-exp-arc-kind z)
	    (format t " (~d)~d" 
		    (ext-exp-arc-kind z)
		    (ext-exp-dag-name (ext-exp-arc-node z)))
	  (format t " ~d" (ext-exp-dag-name (ext-exp-arc-node z))))))
    (format t "~%")
    (dolist (z (ext-exp-dag-arcs d))
      (print-ext-exp-arc-verbose-rec z))))

(defun copy-eed (eed)
  (let ((eed2 (copy-ext-exp-dag eed)))
    (setf (ext-exp-dag-name eed2)
	  (intern-str (create-namestring (read-from-string
					  (string-right-trim "1234567890"
							     (ext-exp-dag-name eed))))))
    eed2))

; leaves play a different role than in exp trees (closer to "empty-dup-info" exp nodes in trees).
; essentially leaves are unmated and may have any wff as a shallow wff
(defun make-eed-leaf (wff pos)
  (make-ext-exp-dag :name (intern-str (create-namestring leaf-name))
		    :kind 'LEAF
		    :positive pos
		    :shallow wff))

(defun make-eed-true (pos)
  (make-ext-exp-dag :name (intern-str (create-namestring true-name))
		    :kind 'TRUE
		    :positive pos
		    :shallow 'TRUTH))

(defun make-eed-false (pos)
  (make-ext-exp-dag :name (intern-str (create-namestring false-name))
		    :kind 'FALSE
		    :positive pos
		    :shallow 'FALSEHOOD))

(defun make-eed-neg (eed)
  (make-ext-exp-dag :name (intern-str (create-namestring neg-name))
		    :kind 'NEG
		    :positive (not (ext-exp-dag-positive eed))
		    :shallow (cons 'NOT (ext-exp-dag-shallow eed))
		    :arcs (list (make-ext-exp-arc :node eed))))

(defun make-eed-con (eed1 eed2)
  (unless (eq (ext-exp-dag-positive eed1) (ext-exp-dag-positive eed2))
    (throwfail "Cannot conjoin " eed1 " and " eed2 " since they have different polarities"))
  (make-ext-exp-dag :name (intern-str (create-namestring econj-name))
		    :kind 'CON
		    :positive (ext-exp-dag-positive eed1)
		    :shallow (acons 'AND (ext-exp-dag-shallow eed1) (ext-exp-dag-shallow eed2))
		    :arcs (list (make-ext-exp-arc :node eed1) (make-ext-exp-arc :node eed2))))

(defun make-eed-dis (eed1 eed2)
  (unless (eq (ext-exp-dag-positive eed1) (ext-exp-dag-positive eed2))
    (throwfail "Cannot disjoin " eed1 " and " eed2 " since they have different polarities"))
  (make-ext-exp-dag :name (intern-str (create-namestring edisj-name))
		    :kind 'DIS
		    :positive (ext-exp-dag-positive eed1)
		    :shallow (acons 'OR (ext-exp-dag-shallow eed1) (ext-exp-dag-shallow eed2))
		    :arcs (list (make-ext-exp-arc :node eed1) (make-ext-exp-arc :node eed2))))

(defun make-eed-imp (eed1 eed2)
  (when (eq (ext-exp-dag-positive eed1) (ext-exp-dag-positive eed2))
    (throwfail "Cannot create imp node from " eed1 " and " eed2 " since they have the same polarity"))
  (make-ext-exp-dag :name (intern-str (create-namestring imp-name))
		    :kind 'IMP
		    :positive (ext-exp-dag-positive eed2)
		    :shallow (acons 'IMPLIES (ext-exp-dag-shallow eed1) (ext-exp-dag-shallow eed2))
		    :arcs (list (make-ext-exp-arc :node eed1) (make-ext-exp-arc :node eed2))))

(defun make-eea-sel (wff pos arcs)
  (make-ext-exp-dag :name (intern-str (create-namestring selection-name))
		    :kind 'SEL
		    :positive pos
		    :shallow wff
		    :arcs arcs))

(defun make-eed-sel (wff pos svl eedl)
  (make-eea-sel wff pos
		(mapcar #'(lambda (x y)
			    (make-ext-exp-arc :kind 'SEL :sel-var x :node y))
			svl eedl)))

(defun make-eed-sel-1 (wff sv eed1)
  (make-eed-sel wff (ext-exp-dag-positive eed1) (list sv) (list eed1)))

(defun make-eea-exp (wff pos arcs)
  (make-ext-exp-dag :name (intern-str (create-namestring expansion-name))
		    :kind 'EXP
		    :positive pos
		    :shallow wff
		    :arcs arcs))

(defun make-eed-exp (wff pos expterms eedl)
  (make-eea-exp
   wff pos
   (mapcar #'(lambda (x y)
	       (make-ext-exp-arc :kind 'EXP :exp-term x :node y))
	   expterms eedl)))

(defun make-eed-exp-1 (wff expterm eed1)
  (make-eed-exp wff (ext-exp-dag-positive eed1) (list expterm) (list eed1)))

(defun make-eed-lambda (wff eed1)
  (let ((wff2 (etanorm (lambda-norm wff))))
    (if (wffeq-ab wff wff2)
	eed1
      (make-eed-rew wff 'LAMBDA eed1))))

(defun make-eed-rew (wff rew-just eed1)
  (make-ext-exp-dag
   :name (intern-str (create-namestring rewrite-name))
   :kind 'REW
   :rew-just rew-just
   :positive (ext-exp-dag-positive eed1)
   :shallow wff
   :arcs (list (make-ext-exp-arc :node eed1))))

(defun make-eed-dec (tp lft rght arcs)
  (if (eq tp 'O)
      (make-eed-mate-1 (acons (inherit-abbrev '= (cons (cons 'O tp) tp) (list tp))
			      lft rght)
		       arcs)
    (make-eed-dec-1 (acons (inherit-abbrev '= (cons (cons 'O tp) tp) (list tp))
			   lft rght)
		    arcs)))

(defun make-eed-dec-1 (wff arcs)
  (make-ext-exp-dag
   :name (intern-str (create-namestring 'DEC))
   :kind 'DEC
   :positive nil
   :shallow wff
   :arcs arcs))

(defun make-eed-dec-2 (wff arcs)
  (make-ext-exp-dag
   :name (intern-str (create-namestring 'EQN))
   :kind 'EQN
   :positive nil
   :shallow wff
   :arcs (list (make-ext-exp-arc :node (make-eed-dec-1 wff arcs)
				 :kind 'EQNDEC))))

(defun make-eed-eunif1 (poseqn negeqn connode)
  (values
   (make-ext-exp-dag :name (intern-str (create-namestring 'EQN))
		     :kind 'EQN
		     :positive t
		     :shallow poseqn
		     :arcs (list (make-ext-exp-arc :kind 'EUNIF1 :node connode)))
   (make-ext-exp-dag :name (intern-str (create-namestring 'EQNGOAL))
		     :kind 'EQNGOAL
		     :positive nil
		     :shallow negeqn
		     :arcs (list (make-ext-exp-arc :kind 'EUNIF1 :node connode)))))

(defun make-eed-eunif2 (poseqn negeqn connode)
  (values
   (make-ext-exp-dag :name (intern-str (create-namestring 'EQN))
		     :kind 'EQN
		     :positive t
		     :shallow poseqn
		     :arcs (list (make-ext-exp-arc :kind 'EUNIF2 :node connode)))
   (make-ext-exp-dag :name (intern-str (create-namestring 'EQNGOAL))
		     :kind 'EQNGOAL
		     :positive nil
		     :shallow negeqn
		     :arcs (list (make-ext-exp-arc :kind 'EUNIF2 :node connode)))))

(defun make-eed-eunif1-2 (poseqn negeqn connode)
  (multiple-value-bind
      (pose nege)
      (make-eed-eunif1 poseqn negeqn connode)
    (values pose
	    (make-ext-exp-dag :name (intern-str (create-namestring 'EQN))
			      :kind 'EQN
			      :positive nil
			      :shallow negeqn
			      :arcs (list (make-ext-exp-arc :kind 'EQNGOAL :node nege))))))

(defun make-eed-eunif2-2 (poseqn negeqn connode)
  (multiple-value-bind
      (pose nege)
      (make-eed-eunif2 poseqn negeqn connode)
    (values pose
	    (make-ext-exp-dag :name (intern-str (create-namestring 'EQN))
			      :kind 'EQN
			      :positive nil
			      :shallow negeqn
			      :arcs (list (make-ext-exp-arc :kind 'EQNGOAL :node nege))))))

(defun make-eed-mate (poswff negwff matenode)
  (values
   (make-ext-exp-dag :name (intern-str (create-namestring 'ATOM))
		     :kind 'ATOM
		     :positive t
		     :shallow poswff
		     :arcs (list (make-ext-exp-arc :kind 'MATE :node matenode)))
   (make-ext-exp-dag :name (intern-str (create-namestring 'ATOM))
		     :kind 'ATOM
		     :positive nil
		     :shallow negwff
		     :arcs (list (make-ext-exp-arc :kind 'MATE :node matenode)))))

(defun make-eed-mate-1 (wff arcs)
  (make-ext-exp-dag :name (intern-str (create-namestring 'DEC))
		    :kind 'DEC
		    :positive nil
		    :shallow wff
		    :arcs arcs))

(defun make-eed-eqo (wff equiv-node)
  (make-eed-rew wff 'EXT= equiv-node))

(defun make-eed-exto (wff equiv-node)
  (make-eed-rew wff 'EXT= equiv-node))

(defun ext-exp-dag-add-rews (rewlist eed)
  (if rewlist
      (let ((just (caar rewlist))
	    (wff (cadr rewlist)))
	(ext-exp-dag-add-rews
	 (cddr rewlist)
	 (case just
	   (AB eed)
	   (LAMBDA (make-eed-rew wff 'LAMBDA eed))
	   ((DEFN BINDER) (make-eed-rew wff 'EQUIVWFFS eed))
	   (t (throwfail "bad rewrite")))))
    eed))

(defun ext-exp-dag-remove-neg (es)
  (let ((sh (ext-exp-dag-shallow es))
	(k (ext-exp-dag-kind es)))
    (unless (not-p sh)
      (throwfail "Not a negated shallow formula"))
    (if (eq k 'NEG)
	(ext-exp-arc-node (car (ext-exp-dag-arcs es)))
      (if (eq k 'REW)
	  (make-eed-rew (cdr sh) (ext-exp-dag-rew-just es)
			(ext-exp-dag-remove-neg (ext-exp-arc-node (car (ext-exp-dag-arcs es)))))
	(if (eq k 'LEAF)
	    (make-eed-leaf (cdr sh) (not (ext-exp-dag-positive es)))
	  (throwfail "Not a negated ext exp dag node"))))))

(defun ext-exp-dag-remove-disjs (es)
  (let ((sh (ext-exp-dag-shallow es))
	(k (ext-exp-dag-kind es)))
    (unless (or-p sh)
      (throwfail "Not a disj shallow formula"))
    (if (eq k 'DIS)
	(list (ext-exp-arc-node (car (ext-exp-dag-arcs es)))
	      (ext-exp-arc-node (cadr (ext-exp-dag-arcs es))))
      (if (eq k 'REW)
	  (let ((l (ext-exp-dag-remove-disjs (ext-exp-arc-node (car (ext-exp-dag-arcs es))))))
	    (list
	     (make-eed-rew (cdar sh) (ext-exp-dag-rew-just es) (car l))
	     (make-eed-rew (cdr sh) (ext-exp-dag-rew-just es) (cadr l))))
	(if (eq k 'LEAF)
	    (list
	     (make-eed-leaf (cdar sh) (ext-exp-dag-positive es))
	     (make-eed-leaf (cdr sh) (ext-exp-dag-positive es)))
	  (throwfail "Not a disj ext exp dag node"))))))

(defun ext-exp-dag-remove-conjs (es)
  (let ((sh (ext-exp-dag-shallow es))
	(k (ext-exp-dag-kind es)))
    (unless (and-p sh)
      (throwfail "Not a conj shallow formula"))
    (if (eq k 'CON)
	(list (ext-exp-arc-node (car (ext-exp-dag-arcs es)))
	      (ext-exp-arc-node (cadr (ext-exp-dag-arcs es))))
      (if (eq k 'REW)
	  (let ((l (ext-exp-dag-remove-conjs (ext-exp-arc-node (car (ext-exp-dag-arcs es))))))
	    (list
	     (make-eed-rew (cdar sh) (ext-exp-dag-rew-just es) (car l))
	     (make-eed-rew (cdr sh) (ext-exp-dag-rew-just es) (cadr l))))
	(if (eq k 'LEAF)
	    (list
	     (make-eed-leaf (cdar sh) (ext-exp-dag-positive es))
	     (make-eed-leaf (cdr sh) (ext-exp-dag-positive es)))
	  (throwfail "Not a disj ext exp dag node"))))))

(defun ext-exp-dag-remove-imps (es)
  (let ((sh (ext-exp-dag-shallow es))
	(k (ext-exp-dag-kind es)))
    (unless (implies-p sh)
      (throwfail "Not an implication shallow formula"))
    (if (eq k 'IMP)
	(list (ext-exp-arc-node (car (ext-exp-dag-arcs es)))
	      (ext-exp-arc-node (cadr (ext-exp-dag-arcs es))))
      (if (eq k 'REW)
	  (let ((l (ext-exp-dag-remove-imps (ext-exp-arc-node (car (ext-exp-dag-arcs es))))))
	    (list
	     (make-eed-rew (cdar sh) (ext-exp-dag-rew-just es) (car l))
	     (make-eed-rew (cdr sh) (ext-exp-dag-rew-just es) (cadr l))))
	(if (eq k 'LEAF)
	    (list
	     (make-eed-leaf (cdar sh) (not (ext-exp-dag-positive es)))
	     (make-eed-leaf (cdr sh) (ext-exp-dag-positive es)))
	  (throwfail "Not an implication ext exp dag node"))))))

(defun ext-exp-dag-remove-exps (es)
  (let ((sh (ext-exp-dag-shallow es))
	(k (ext-exp-dag-kind es)))
    (unless (ae-bd-wff-p sh)
      (throwfail "Not a quantified shallow formula"))
    (if (eq k 'EXP)
	(let ((eearcs (ext-exp-dag-arcs es))
	      (ret nil))
	  (dolist (a eearcs)
	    (push (cons (ext-exp-arc-exp-term a) (ext-exp-arc-node a)) ret))
	  (reverse ret))
      (if (eq k 'REW)
	  (let ((l (ext-exp-dag-remove-exps (ext-exp-arc-node (car (ext-exp-dag-arcs es)))))
		(v (bindvar sh))
		(body (cdr sh))
		(pos (ext-exp-dag-positive es))
		(rj (ext-exp-dag-rew-just es)))
	    (mapcar #'(lambda (x)
			(cons (car x)
			      (make-eed-rew
			       (substitute-l-term-var (car x) v body)
			       rj (cdr x))))
		    l))
	(if (eq k 'LEAF)
	    nil
	  (throwfail "Not an expansion node"))))))

(defun ext-exp-dag-remove-sels (es)
  (let ((sh (ext-exp-dag-shallow es))
	(k (ext-exp-dag-kind es)))
    (unless (ae-bd-wff-p sh)
      (throwfail "Not a quantified shallow formula"))
    (if (eq k 'SEL)
	(let* ((eearcs (ext-exp-dag-arcs es))
	       (ret nil))
	  (dolist (a eearcs)
	    (push (cons (ext-exp-arc-sel-var a) (ext-exp-arc-node a)) ret))
	  (reverse ret))
      (if (eq k 'REW)
	  (let ((l (ext-exp-dag-remove-sels (ext-exp-arc-node (car (ext-exp-dag-arcs es)))))
		(v (bindvar sh))
		(body (cdr sh))
		(pos (ext-exp-dag-positive es))
		(rj (ext-exp-dag-rew-just es)))
	    (mapcar #'(lambda (x)
			(cons (car x)
			      (make-eed-rew
			       (substitute-l-term-var (car x) v body)
			       rj (cdr x))))
		    l))
	(if (eq k 'LEAF)
	    nil
	  (throwfail "Not a selection node"))))))

(defun ext-exp-dag-remove-equiv (equivnode)
  (let ((k (ext-exp-dag-kind equivnode))
	(sh (ext-exp-dag-shallow equivnode)))
    (unless (equiv-p sh)
      (throwfail "Not an equivalence node"))
    (if (eq k 'REW)
	(let ((rj (ext-exp-dag-rew-just equivnode)))
	  (if (eq rj 'EQUIV-IMPLICS)
	      (ext-exp-dag-remove-equiv-implics-1
	       (ext-exp-arc-node (car (ext-exp-dag-arcs equivnode))))
	    (if (eq rj 'EQUIV-DISJS)
		(ext-exp-dag-remove-equiv-disjs-1
		 (ext-exp-arc-node (car (ext-exp-dag-arcs equivnode))))
	      (ext-exp-dag-remove-equiv
	       (ext-exp-arc-node (car (ext-exp-dag-arcs equivnode)))))))
      (if (eq k 'LEAF)
	  (let ((a (cdar sh))
		(b (cdr sh))
		(pos (ext-exp-dag-positive equivnode)))
	    (cons (cons (make-eed-leaf a pos) (make-eed-leaf b pos))
		  (cons (make-eed-leaf a (not pos)) (make-eed-leaf b (not pos)))))
	(throwfail "Not an equivalence node")))))

(defun ext-exp-dag-remove-equiv-implics-1 (connode)
  (let ((k (ext-exp-dag-kind connode))
	(sh (ext-exp-dag-shallow connode)))
    (unless (and-p sh)
      (throwfail "Not a conjunction node"))
    (if (eq k 'CON)
	(let ((imps1 (ext-exp-dag-remove-imps (ext-exp-arc-node (car (ext-exp-dag-arcs connode)))))
	      (imps2 (ext-exp-dag-remove-imps (ext-exp-arc-node (cadr (ext-exp-dag-arcs connode))))))
	  (cons (cons (cadr imps2) (cadr imps1))
		(cons (car imps1) (car imps2))))
      (if (eq k 'REW)
	  (ext-exp-dag-remove-equiv-implics-1
	   (ext-exp-arc-node (car (ext-exp-dag-arcs connode))))
	(if (eq k 'LEAF)
	    (let* ((imp1 (cdar sh))
		   (imp2 (cdr sh))
		   (a1 (cdar imp1))
		   (b1 (cdr imp1))
		   (b2 (cdar imp2))
		   (a2 (cdr imp2))
		   (pos (ext-exp-dag-positive connode)))
	      (cons (cons (make-eed-leaf a2 pos) (make-eed-leaf b1 pos))
		    (cons (make-eed-leaf a1 (not pos)) (make-eed-leaf b2 (not pos)))))
	  (throwfail "Not a conjunction node"))))))

(defun ext-exp-dag-remove-equiv-disjs-1 (disnode)
  (let ((k (ext-exp-dag-kind disnode))
	(sh (ext-exp-dag-shallow disnode)))
    (unless (or-p sh)
      (throwfail "Not a disjunction node"))
    (if (eq k 'DIS)
	(let ((conjs1 (ext-exp-dag-remove-conjs (ext-exp-arc-node (car (ext-exp-dag-arcs disnode)))))
	      (conjs2 (ext-exp-dag-remove-conjs (ext-exp-arc-node (cadr (ext-exp-dag-arcs disnode))))))
	  (cons (cons (car conjs1) (cadr conjs1))
		(cons (ext-exp-dag-remove-neg (car conjs2))
		      (ext-exp-dag-remove-neg (cadr conjs2)))))
      (if (eq k 'REW)
	  (ext-exp-dag-remove-equiv-disjs-1
	   (ext-exp-arc-node (car (ext-exp-dag-arcs disnode))))
	(if (eq k 'LEAF)
	    (let* ((con1 (cdar sh))
		   (con2 (cdr sh))
		   (a1 (cdar con1))
		   (b1 (cdr con1))
		   (a2 (cddar con2))
		   (b2 (cddr con2))
		   (pos (ext-exp-dag-positive disnode)))
	      (cons (cons (make-eed-leaf a1 pos) (make-eed-leaf b1 pos))
		    (cons (make-eed-leaf a2 (not pos)) (make-eed-leaf b2 (not pos)))))
	  (throwfail "Not a disjunction node"))))))

(defun ext-exp-dag-contract-defns (edags-assoc)
  (let ((node-assoc nil)
	(node-delayed nil)
	(edags-assoc2 nil))
    (declare (special node-assoc node-delayed))
    (dolist (sh-eed edags-assoc)
      (ext-exp-dag-contract-defns-1 (car sh-eed) (cdr sh-eed)))
    (dolist (sh-eed edags-assoc)
      (push (cons (car sh-eed)
		  (ext-exp-dag-contract-defns-2 (cdr sh-eed)))
	    edags-assoc2))
    (reverse edags-assoc2)))

(defun ext-exp-dag-contract-defns-1 (sh eed)
  (declare (special node-assoc))
  (let ((na (assoc eed node-assoc)))
    (unless na
      (push (cons eed sh) node-assoc)
      (ext-exp-dag-contract-defns-1-1 sh eed))))

(defun ext-exp-dag-contract-defns-2 (eed)
  (declare (special node-assoc))
  (let ((na (assoc eed node-assoc)))
    (if (ext-exp-dag-p (cdr na))
	(cdr na)
      (let ((eed2 (ext-exp-dag-contract-defns-2-1 (cdr na) eed)))
	(when ext-exp-dag-debug
	  (unless (ext-exp-dag-check-structure-l (list eed2))
	    (setf (get 'ext-exp-dag-debug 'eed) eed)
	    (setf (get 'ext-exp-dag-debug 'eed2) eed2)
	    (throwfail "Structure Problem in ext-exp-dag-contract-defns")))
	eed2))))

(defun ext-exp-dag-contract-defns-2-1 (sh eed)
  (let* ((nwff (etanorm (lambda-norm sh)))
	 (eed2 (ext-exp-dag-contract-defns-2-2 nwff eed)))
    (if (wffeq-ab sh nwff)
	eed2
      (make-eed-rew sh 'LAMBDA eed2))))

(defun ext-exp-dag-contract-defns-2-2 (sh eed)
  (if (atom-head-abbrev-p sh)
      (make-eed-rew sh 'EQUIVWFFS
		    (ext-exp-dag-contract-defns-2-1 (instantiate-head-abbrev sh) eed))
    (ext-exp-dag-contract-defns-2-3 sh eed)))

(defun ext-exp-dag-contract-defns-2-3 (sh eed)
  (let ((eed2 (copy-eed eed)))
    (setf (ext-exp-dag-shallow eed2) sh)
    (setf (ext-exp-dag-arcs eed2)
	  (mapcar #'(lambda (arc)
		      (ext-exp-arc-contract-defns-2 arc))
		  (ext-exp-dag-arcs eed)))
    (push (cons eed eed2) node-assoc)
    eed2))

(defun ext-exp-arc-contract-defns-2 (arc)
  (let ((arc2 (copy-ext-exp-arc arc)))
    (setf (ext-exp-arc-node arc2)
	  (ext-exp-dag-contract-defns-2 (ext-exp-arc-node arc)))
    arc2))

(defun ext-exp-dag-contract-defns-1-1 (sh eed)
  (let ((sh2 (etanorm (lambda-norm sh))))
    (if (atom-head-abbrev-p sh2)
	(ext-exp-dag-contract-defns-1-1 (instantiate-head-abbrev sh2) eed)
      (ext-exp-dag-contract-defns-1-2 sh2 eed))))

(defun ext-exp-dag-contract-defns-1-2 (sh eed)
  (declare (special node-delayed))
  (let ((k (ext-exp-dag-kind eed))
	(arcs (ext-exp-dag-arcs eed)))
    (case k
      (NEG (ext-exp-dag-contract-defns-1 (cdr sh) (ext-exp-arc-node (car arcs))))
      ((CON DIS IMP)
       (ext-exp-dag-contract-defns-1 (cdar sh) (ext-exp-arc-node (car arcs)))
       (ext-exp-dag-contract-defns-1 (cdr sh) (ext-exp-arc-node (cadr arcs))))
      (EXP
       (let ((x (bindvar sh))
	     (body (cdr sh)))
	 (dolist (arc (ext-exp-dag-arcs eed))
	   (let ((exp (ext-exp-arc-exp-term arc)))
	     (ext-exp-dag-contract-defns-1
	      (substitute-l-term-var exp x body)
	      (ext-exp-arc-node arc))))))
      (SEL
       (let ((x (bindvar sh))
	     (body (cdr sh)))
	 (dolist (arc (ext-exp-dag-arcs eed))
	   (let ((sv (ext-exp-arc-sel-var arc)))
	     (ext-exp-dag-contract-defns-1
	      (substitute-l-term-var sv x body)
	      (ext-exp-arc-node arc))))))
      (ATOM
       (dolist (arc (ext-exp-dag-arcs eed))
	 (let* ((node (ext-exp-arc-node arc))
		(na (assoc node node-delayed)))
	   (if na
	       (let* ((q (inherit-abbrev '= (cons (cons 'O 'O) 'O) (list 'O)))
		      (sh2 (cdr na))
		      (sh3 (if (ext-exp-dag-positive eed)
			       (acons q sh2 sh)
			     (acons q sh sh2))))
		 (ext-exp-dag-contract-defns-1 sh3 node))
	     (push (cons node sh) node-delayed)))))
      (DEC
       (dolist (arc arcs)
	 (let* ((lft (cdar sh))
		(rght (cdr sh))
		(args1 (args lft))
		(args2 (args rght))
		(i (ext-exp-arc-dec-index arc))
		(a1 (nth i args1))
		(a2 (nth i args2))
		(tp (unabbreviated-type a1)))
	   (ext-exp-dag-contract-defns-1
	    (acons (inherit-abbrev '= (cons (cons 'O tp) tp) (list tp)) a1 a2)
	    (ext-exp-arc-node arc)))))
      ((EQN EQNGOAL)
       (if (or (ext-exp-dag-positive eed) (eq k 'EQNGOAL))
	   (dolist (arc arcs)
	     (let* ((node (ext-exp-arc-node arc))
		    (na (assoc node node-delayed)))
	       (if na
		   (let* ((sh2 (cdr na))
			  (lft (cdar sh))
			  (rght (cdr sh))
			  (lft2 (cdar sh2))
			  (rght2 (cdr sh2))
			  (tp (unabbreviated-type lft))
			  (q (inherit-abbrev '= (cons (cons 'O tp) tp) (list tp))))
		     (ext-exp-dag-contract-defns-1
		      (if (ext-exp-dag-positive eed)
			  (if (eq (ext-exp-arc-kind arc) 'EUNIF1)
			      (acons 'AND
				     (acons q lft lft2)
				     (acons q rght rght2))
			    (acons 'AND
				   (acons q lft rght2)
				   (acons q rght lft2)))
			(if (eq (ext-exp-arc-kind arc) 'EUNIF1)
			    (acons 'AND
				   (acons q lft2 lft)
				   (acons q rght2 rght))
			  (acons 'AND
				 (acons q lft2 rght)
				 (acons q rght2 lft))))
		      node))
		 (push (cons node sh) node-delayed))))
	 (dolist (arc arcs)
	   (ext-exp-dag-contract-defns-1 sh (ext-exp-arc-node arc)))))
      (REW
       (case (ext-exp-dag-rew-just eed)
	 (EQUIVWFFS (throwfail "Should not be an EQUIVWFFS rewrite node in contract-defns"))
	 (EQUIV-IMPLICS
	  (let* ((lft (cdar sh))
		 (rght (cdr sh))
		 (sh2 (acons 'AND
			     (acons 'IMPLIES lft rght)
			     (acons 'IMPLIES rght lft))))
	    (ext-exp-dag-contract-defns-1 sh2 (ext-exp-arc-node (car (ext-exp-dag-arcs eed))))))
	 (EQUIV-DISJS
	  (let* ((lft (cdar sh))
		 (rght (cdr sh))
		 (sh2 (acons 'OR
			     (acons 'AND lft rght)
			     (acons 'AND (cons 'NOT lft) (cons 'NOT rght)))))
	    (ext-exp-dag-contract-defns-1 sh2 (ext-exp-arc-node (car (ext-exp-dag-arcs eed))))))
	 (LAMBDA (ext-exp-dag-contract-defns-1 sh (ext-exp-arc-node (car (ext-exp-dag-arcs eed)))))
	 (EXT=
	  (let* ((lft (cdar sh))
		 (rght (cdr sh))
		 (tp (unabbreviated-type lft))
		 (sh2 (if (eq tp 'O)
			  (acons 'EQUIV lft rght)
			(let ((x (fresh-var-1 (cdr tp)))
			      (q (inherit-abbrev '= (cons (cons 'O (car tp)) (car tp)) (list (car tp)))))
			  (acons x 'FORALL (acons q (cons lft x) (cons rght x)))))))
	    (ext-exp-dag-contract-defns-1 sh2 (ext-exp-arc-node (car (ext-exp-dag-arcs eed))))))))
      (t nil))))

(defun cutfree-ext-seq-to-ext-exp-dags (es)
  (declare (special *first-equiv*))
  (let ((wff-hash (make-hash-table :test #'equal)))
    (declare (special wff-hash))
    (setq *first-equiv* t)
    (let* ((edags-assoc (cutfree-ext-seq-to-ext-exp-dags-1 es))
	   (edags (mapcar #'cdr edags-assoc))
	   (edags2 (ext-exp-dag-merge-list edags))
	   (edags2-assoc (mapcar #'(lambda (x y)
				     (cons (car x) y))
				 edags-assoc
				 edags2))
	   (edags3-assoc (ext-exp-dag-contract-defns edags2-assoc)))
      (when ext-exp-dag-debug
	(unless (ext-exp-dag-check-pf-p (mapcar #'cdr edags3-assoc)) ; now we've contracted defns in the edag
	  (setf (get 'ext-exp-dag-debug 'edags) edags)
	  (throwfail "Bug in Translation from cutfree ext-seq derivation to ext-exp-dags")))
      edags3-assoc)))

(defun cutfree-ext-seq-to-ext-exp-dags-1 (es)
  (let ((edags (cutfree-ext-seq-to-ext-exp-dags-2 es))
	(a t))
    (when ext-exp-dag-debug
      (unless (ext-exp-dag-check-pf-p (mapcar #'cdr edags) :rewrite-defns-eager T) ; automatically expand abbrev's in exp terms
	(setf (get 'ext-exp-dag-debug 'es) es)
	(setf (get 'ext-exp-dag-debug 'edags) edags)
	(throwfail "Bug in Translation from cutfree ext-seq derivation " es " to ext-exp-dags")))
    edags))

(defun ext-seq-to-eed-instantiate-all (w)
  (declare (special wff-hash))
  (let ((w2 (gethash w wff-hash)))
    (if w2
	w2
      (let ((w3 (etanorm (lambda-norm (instantiate-all-rec w '(EQUIV))))))
	(setf (gethash w wff-hash) w3)
	w3))))

(defun make-eed-nonatom-mates (w)
  (let* ((es (ext-seq-expand-init w w))
	 (edags (cutfree-ext-seq-to-ext-exp-dags-2 es))
	 (pos (assoc (cons 'NOT w) edags :test #'wffeq-ab))
	 (neg (assoc w edags :test #'wffeq-ab)))
    (unless (and pos neg)
      (throwfail "problem making nonatom mates for " (w . gwff)))
    (values (ext-exp-dag-remove-neg (cdr pos)) (cdr neg))))

(defun cutfree-ext-seq-to-ext-exp-dags-2 (es)
  (let* ((princ-wffs (ext-seq-princ-wffs es))
	 (wffs (ext-seq-wffs es))
	 (pos (ext-seq-pos-rule es))
	 (k (ext-seq-kind es))
	 (prems (ext-seq-prems es))
	 (edprems nil))
    (unless (member k '(CUT INIT REFL))
      (setq edprems (mapcar #'cutfree-ext-seq-to-ext-exp-dags-1 prems)))
    (case k
      (TRUE
       (let ((tr (make-eed-true nil)))
	 (mapcar #'(lambda (w)
		     (if (eq w 'TRUTH)
			 (cons w tr)
		       (let ((w2 (ext-seq-to-eed-instantiate-all w)))
			 (cons w (make-eed-leaf w2 nil)))))
		 wffs)))
      (NEG
       (let* ((ret nil)
	      (eed1 nil)
	      (edprems1 (car edprems))
	      (wffs1 (ext-seq-wffs (car prems)))
	      (pwff (car princ-wffs))
	      (pwff1 (cddr pwff)))
	 (dolist (w wffs1)
	   (let ((a (assoc w edprems1 :test #'wffeq-ab)))
	     (unless a
	       (throwfail "problem building ext-exp-dag"))
	     (setq edprems1 (remove a edprems1 :count 1))
	     (if (and (wffeq-ab w pwff1) (not eed1))
		 (setq eed1 (cdr a))
	       (push a ret))))
	 (unless eed1
	   (throwfail "problem building ext-exp-dag"))
	 (push (cons pwff (make-eed-neg (make-eed-neg eed1))) ret)
	 ret))
      (DIS
       (if pos
	   (let* ((ret nil)
		  (eed1 nil)
		  (eed2 nil)
		  (edprems1 (car edprems))
		  (wffs1 (ext-seq-wffs (car prems)))
		  (pwff (car princ-wffs))
		  (pwff1 (cdar pwff))
		  (pwff2 (cdr pwff)))
	     (dolist (w wffs1)
	       (let ((a (assoc w edprems1 :test #'wffeq-ab)))
		 (unless a
		   (throwfail "problem building ext-exp-dag"))
		 (setq edprems1 (remove a edprems1 :count 1))
		 (if (and (wffeq-ab w pwff1) (not eed1))
		     (setq eed1 (cdr a))
		   (if (and (wffeq-ab w pwff2) (not eed2))
		       (setq eed2 (cdr a))
		     (push a ret)))))
	     (unless (and eed1 eed2)
	       (throwfail "problem building ext-exp-dag"))
	     (push (cons pwff (make-eed-dis eed1 eed2)) ret)
	     ret)
	 (let* ((ret nil)
		(eed1 nil)
		(eed2 nil)
		(edprems1 (car edprems))
		(edprems2 (cadr edprems))
		(wffs1 (ext-seq-wffs (car prems)))
		(pwff (car princ-wffs))
		(pwff1 (cons 'NOT (cdadr pwff)))
		(pwff2 (cons 'NOT (cddr pwff))))
	   (dolist (w wffs1)
	       (let ((a (assoc w edprems1 :test #'wffeq-ab)))
		 (unless a
		   (throwfail "problem building ext-exp-dag"))
		 (setq edprems1 (remove a edprems1 :count 1))
		 (if (and (wffeq-ab w pwff1) (not eed1))
		     (setq eed1 (ext-exp-dag-remove-neg (cdr a)))
		   (let ((b (assoc w edprems2 :test #'wffeq-ab)))
		     (unless b
		       (throwfail "problem building ext-exp-dag"))
		     (setq edprems2 (remove b edprems2 :count 1))
		     (push (cons w (ext-exp-dag-pre-merge (cdr a) (cdr b))) ret)))))
	   (unless (and eed1 edprems2 (not (cdr edprems2))
			(wffeq-ab pwff2 (caar edprems2)))
	     (throwfail "problem building ext-exp-dag"))
	   (setq eed2 (ext-exp-dag-remove-neg (cdar edprems2)))
	   (push (cons pwff (make-eed-neg (make-eed-dis eed1 eed2))) ret)
	   ret)))
      (FORALL
       (if pos
	   (let* ((ret nil)
		  (eed1 nil)
		  (edprems1 (car edprems))
		  (wffs1 (ext-seq-wffs (car prems)))
		  (pwff (car princ-wffs))
		  (sv (ext-seq-sel-var es))
		  (pwff1 (substitute-l-term-var sv (bindvar pwff) (cdr pwff))))
	     (dolist (w wffs1)
	       (let ((a (assoc w edprems1 :test #'wffeq-ab)))
		 (unless a
		   (throwfail "problem building ext-exp-dag"))
		 (setq edprems1 (remove a edprems1 :count 1))
		 (if (and (wffeq-ab w pwff1) (not eed1))
		     (setq eed1 (cdr a))
		   (push a ret))))
	     (unless eed1
	       (throwfail "problem building ext-exp-dag"))
	     (push (cons pwff (make-eed-sel-1 (ext-seq-to-eed-instantiate-all pwff) sv eed1)) ret)
	     ret)
	 (let* ((ret nil)
		(eed1 nil)
		(edprems1 (car edprems))
		(wffs1 (ext-seq-wffs (car prems)))
		(pwff (car princ-wffs))
		(exp (ext-seq-exp-term es))
		(exp00 (ext-seq-to-eed-instantiate-all exp))
		(pwff00 (ext-seq-to-eed-instantiate-all (cdr pwff)))
		(pwff1 (cons 'NOT (substitute-l-term-var exp (bindvar (cdr pwff)) (cddr pwff))))
		(pwff10 (substitute-l-term-var exp00 (bindvar pwff00) (cdr pwff00))))
	   (dolist (w wffs1)
	     (let ((a (assoc w edprems1 :test #'wffeq-ab)))
	       (unless a
		 (throwfail "problem building ext-exp-dag"))
	       (setq edprems1 (remove a edprems1 :count 1))
	       (if (and (wffeq-ab w pwff1) (not eed1))
		   (setq eed1 (ext-exp-dag-remove-neg (cdr a)))
		 (push a ret))))
	   (unless eed1
	     (throwfail "problem building ext-exp-dag"))
	   (push (cons pwff (make-eed-neg (make-eed-exp-1 pwff00 exp
							  (make-eed-lambda pwff10 eed1)))) ret)
	   ret)))
      (INTERNALIZE
       (let* ((ret nil)
	      (eed1 nil)
	      (edprems1 (car edprems))
	      (wffs1 (ext-seq-wffs (car prems)))
	      (pwff (car princ-wffs))
	      (pwff1 (if pos pwff (cdr pwff)))
	      (ewff (externalize-wff1 pwff1))
	      (ewff2 (if pos ewff (cons 'NOT ewff)))
	      (pwff1-node nil))
	 (dolist (w wffs1)
	   (let ((a (assoc w edprems1 :test #'wffeq-ab)))
	     (unless a
	       (throwfail "problem building ext-exp-dag"))
	     (setq edprems1 (remove a edprems1 :count 1))
	     (if (and (wffeq-ab w ewff2) (not eed1))
		 (setq eed1 (cdr a))
	       (push a ret))))
	 (unless eed1
	   (throwfail "problem building ext-exp-dag"))
	 (unless pos
	   (setq eed1 (ext-exp-dag-remove-neg eed1)))
	 (cond ((and-p pwff1)
		(let* ((eed2 (ext-exp-dag-remove-neg eed1))
		       (eed34 (ext-exp-dag-remove-disjs eed2))
		       (eed3 (ext-exp-dag-remove-neg (car eed34)))
		       (eed4 (ext-exp-dag-remove-neg (cadr eed34))))
		  (setq pwff1-node (make-eed-con eed3 eed4))))
	       ((implies-p pwff1)
		(let ((eed34 (ext-exp-dag-remove-disjs eed1)))
		  (setq pwff1-node (make-eed-imp
				    (ext-exp-dag-remove-neg (car eed34))
				    (cadr eed34)))))
	       ((equiv-p pwff1)
		(let* ((eed2 (ext-exp-dag-remove-neg eed1))
		       (eed34 (ext-exp-dag-remove-disjs eed2))
		       (eed3 (car eed34))
		       (eed4 (cadr eed34))
		       (eed5 (ext-exp-dag-remove-neg eed3))
		       (eed6 (ext-exp-dag-remove-neg eed4))
		       (eed78 (ext-exp-dag-remove-disjs eed5))
		       (eed90 (ext-exp-dag-remove-disjs eed6))
		       (eed7 (ext-exp-dag-remove-neg (car eed78)))
		       (eed8 (cadr eed78))
		       (eed9 (car eed90))
		       (eed10 (ext-exp-dag-remove-neg (cadr eed90)))
		       (rewrite-as-conj
			(case rewrite-equivs
			  (2 (not pos))
			  (4 t)
			  (3 (if *first-equiv* pos (not pos)))
			  (5 nil)
			  (t pos))))
		  (setq *first-equiv* nil)
		  (setq pwff1-node
			(if rewrite-as-conj
			    (make-eed-rew (ext-seq-to-eed-instantiate-all pwff1)
					  'EQUIV-IMPLICS
					  (make-eed-con
					   (make-eed-imp eed7 eed8)
					   (make-eed-imp eed10 eed9)))
			  (if pos
			      (multiple-value-bind
				  (eed07 eed09)
				  (make-eed-nonatom-mates (ext-exp-dag-shallow eed7))
				(multiple-value-bind
				    (eed010 eed08)
				    (make-eed-nonatom-mates (ext-exp-dag-shallow eed8))
				  (let* ((eed17 (ext-exp-dag-pre-merge eed7 eed07))
					 (eed18 (ext-exp-dag-pre-merge eed8 eed08))
					 (eed19 (ext-exp-dag-pre-merge eed9 eed09))
					 (eed110 (ext-exp-dag-pre-merge eed10 eed010)))
				    (make-eed-rew (ext-seq-to-eed-instantiate-all pwff1)
						  'EQUIV-DISJS
						  (make-eed-dis
						   (make-eed-con eed19 eed18)
						   (make-eed-con (make-eed-neg eed17)
								 (make-eed-neg eed110)))))))
			    (make-eed-rew (ext-seq-to-eed-instantiate-all pwff1)
					  'EQUIV-DISJS
					  (make-eed-dis
					   (make-eed-con eed9 eed8)
					   (make-eed-con (make-eed-neg eed7)
							 (make-eed-neg eed10)))))))))
	       ((e-bd-wff-p pwff1)
		(if pos
		    (let* ((eed2 (ext-exp-dag-remove-neg eed1))
			   (l (ext-exp-dag-remove-exps eed2))
			   (expterms (mapcar #'car l))
			   (nodes (mapcar #'(lambda (x)
					      (ext-exp-dag-remove-neg (cdr x))) l)))
		      (setq pwff1-node (make-eed-exp (ext-seq-to-eed-instantiate-all pwff1)
						     nil expterms nodes)))
		  (let* ((eed2 (ext-exp-dag-remove-neg eed1))
			 (l (ext-exp-dag-remove-sels eed2))
			 (svl (mapcar #'car l))
			 (nodes (mapcar #'(lambda (x)
					    (ext-exp-dag-remove-neg (cdr x))) l)))
		    (setq pwff1-node (make-eed-sel (ext-seq-to-eed-instantiate-all pwff1)
						   t svl nodes)))))
	       ((eq pwff1 'FALSEHOOD)
		(setq pwff1-node (make-eed-false (not pos))))
	       (t (setq pwff1-node eed1)))
	 (if pos
	     (push (cons pwff pwff1-node) ret)
	   (push (cons pwff (make-eed-neg pwff1-node)) ret))
	 ret))
      (LAMBDA
       (let* ((ret nil)
	      (eed1 nil)
	      (edprems1 (car edprems))
	      (wffs1 (ext-seq-wffs (car prems)))
	      (pwff (car princ-wffs))
	      (pwff1 (etanorm (lambda-norm pwff))))
	 (dolist (w wffs1)
	   (let ((a (assoc w edprems1 :test #'wffeq-ab)))
	     (unless a
	       (throwfail "problem building ext-exp-dag"))
	     (setq edprems1 (remove a edprems1 :count 1))
	     (if (and (wffeq-ab w pwff1) (not eed1))
		 (setq eed1 (cdr a))
	       (push a ret))))
	 (unless eed1
	   (throwfail "problem building ext-exp-dag"))
	 (push (cons pwff eed1) ret)
	 ret))
      (INITEQ
       (let* ((ret nil)
	      (eedl nil)
	      (pwff (car princ-wffs))
	      (pwff1 (cdr pwff))
	      (pwff2 (cadr princ-wffs))
	      (args1 (args pwff1))
	      (args2 (args pwff2))
	      (fst t)
	      (i -1))
	 (dolist (epa (mapcar #'list edprems prems args1 args2))
	   (let* ((edprems1 (car epa))
		  (prem1 (cadr epa))
		  (wffs1 (ext-seq-wffs prem1))
		  (arg1 (caddr epa))
		  (arg2 (caddr (cdr epa)))
		  (tp (type arg1))
		  (ewff (acons (inherit-abbrev '= (cons (cons 'O tp) tp) (list tp))
			       arg2 arg1))
		  (eed1 nil))
	     (dolist (w wffs1)
	       (let ((a (assoc w edprems1 :test #'wffeq-ab)))
		 (unless a
		   (throwfail "problem building ext-exp-dag"))
		 (setq edprems1 (remove a edprems1 :count 1))
		 (if (and (wffeq-ab w ewff) (not eed1))
		     (setq eed1 (cdr a))
		   (if fst
		       (push a ret)
		     (let ((b (assoc w ret :test #'wffeq-ab))
			   (eed9 (cdr a)))
		       (unless b
			 (throwfail "problem building ext-exp-dag"))
		       (setq ret (remove b ret :count 1))
		       (push (cons w (ext-exp-dag-pre-merge eed9 (cdr b)))
			     ret))))))
	     (unless eed1
	       (throwfail "problem building ext-exp-dag"))
	     (setq fst nil)
	     (push eed1 eedl)))
	 (when fst ; i.e., there were no arguments
	   (let ((gam (multiset-extract-wffs wffs pwff pwff2)))
	     (dolist (w gam)
	       (push (cons w (make-eed-leaf (ext-seq-to-eed-instantiate-all w) nil)) ret))))
	 (let* ((earcs (mapcar #'(lambda (x)
				   (make-ext-exp-arc :kind 'DEC
						     :dec-index (incf i)
						     :node x))
			       (reverse eedl)))
		(pwff10 (ext-seq-to-eed-instantiate-all pwff1))
		(pwff20 (ext-seq-to-eed-instantiate-all pwff2))
		(decnode (make-eed-dec 'O pwff20 pwff10 earcs)))
	   (multiple-value-bind
	       (posnode negnode)
	       (make-eed-mate pwff10 pwff20 decnode)
	     (push (cons pwff (make-eed-neg posnode)) ret)
	     (push (cons pwff2 negnode) ret)))
	 ret))
      (DEC
       (let* ((ret nil)
	      (eedl nil)
	      (pwff (car princ-wffs))
	      (pwff1 (cdar pwff))
	      (pwff2 (cdr pwff))
	      (args1 (args pwff1))
	      (args2 (args pwff2))
	      (i -1)
	      (fst t))
	 (dolist (epa (mapcar #'list edprems prems args1 args2))
	   (let* ((edprems1 (car epa))
		  (prem1 (cadr epa))
		  (wffs1 (ext-seq-wffs prem1))
		  (arg1 (caddr epa))
		  (arg2 (caddr (cdr epa)))
		  (tp (type arg1))
		  (ewff (acons (inherit-abbrev '= (cons (cons 'O tp) tp) (list tp))
			       arg1 arg2))
		  (eed1 nil))
	     (dolist (w wffs1)
	       (let ((a (assoc w edprems1 :test #'wffeq-ab)))
		 (unless a
		   (throwfail "problem building ext-exp-dag"))
		 (setq edprems1 (remove a edprems1 :count 1))
		 (if (and (wffeq-ab w ewff) (not eed1))
		     (setq eed1 (cdr a))
		   (if fst
		       (push a ret)
		     (let ((b (assoc w ret :test #'wffeq-ab))
			   (eed9 (cdr a)))
		       (unless b
			 (throwfail "problem building ext-exp-dag"))
		       (setq ret (remove b ret :count 1))
		       (push (cons w (ext-exp-dag-pre-merge eed9 (cdr b)))
			     ret))))))
	     (unless eed1
	       (throwfail "problem building ext-exp-dag"))
	     (setq fst nil)
	     (push eed1 eedl)))
	 (when fst ; i.e., there were no arguments
	   (let ((gam (multiset-extract-wffs wffs pwff)))
	     (dolist (w gam)
	       (push (cons w (make-eed-leaf (ext-seq-to-eed-instantiate-all w) nil)) ret))))
	 (let* ((earcs (mapcar #'(lambda (x)
				   (make-ext-exp-arc :kind 'DEC
						     :dec-index (incf i)
						     :node x))
			       (reverse eedl)))
		(decnode (make-eed-dec-2 (ext-seq-to-eed-instantiate-all pwff) earcs)))
	   (push (cons pwff decnode) ret))
	 ret))
      ((EUNIF1 EUNIF2)
       (let* ((ret nil)
	      (edprems1 (car edprems))
	      (edprems2 (cadr edprems))
	      (wffs1 (ext-seq-wffs (car prems)))
	      (wffs2 (ext-seq-wffs (cadr prems)))
	      (eed1 nil)
	      (eed2 nil)
	      (pwff (car princ-wffs))
	      (pwff1 (cdr pwff))
	      (pwff2 (cadr princ-wffs))
	      (lft1 (cdar pwff1))
	      (rght1 (cdr pwff1))
	      (lft2 (cdar pwff2))
	      (rght2 (cdr pwff2))
	      (tp (type lft1))
	      (q (inherit-abbrev '= (cons (cons 'O tp) tp) (list tp)))
	      (ewff1 (if (eq k 'EUNIF1)
			 (acons q lft1 lft2)
		       (acons q lft1 rght2)))
	      (ewff2 (if (eq k 'EUNIF1)
			 (acons q rght1 rght2)
		       (acons q rght1 lft2))))
	 (dolist (w wffs1)
	   (let ((a (assoc w edprems1 :test #'wffeq-ab)))
	     (unless a
	       (throwfail "problem building ext-exp-dag"))
	     (setq edprems1 (remove a edprems1 :count 1))
	     (if (and (wffeq-ab w ewff1) (not eed1))
		 (setq eed1 (cdr a))
	       (let ((b (assoc w edprems2 :test #'wffeq-ab)))
		 (unless b
		   (throwfail "problem building ext-exp-dag"))
		 (setq edprems2 (remove b edprems2 :count 1))
		 (push (cons w (ext-exp-dag-pre-merge (cdr a) (cdr b))) ret)))))
	 (unless (and eed1 edprems2 (not (cdr edprems2))
		      (wffeq-ab ewff2 (caar edprems2)))
	   (throwfail "problem building ext-exp-dag"))
	 (setq eed2 (cdar edprems2))
	 (let ((pwff10 (ext-seq-to-eed-instantiate-all pwff1))
	       (pwff20 (ext-seq-to-eed-instantiate-all pwff2)))
	   (multiple-value-bind
	       (poseqn negeqn)
	       (if (eq k 'EUNIF1)
		   (make-eed-eunif1-2 pwff10 pwff20 (make-eed-con eed1 eed2))
		 (make-eed-eunif2-2 pwff10 pwff20 (make-eed-con eed1 eed2)))
	     (push (cons pwff (make-eed-neg poseqn)) ret)
	     (push (cons pwff2 negeqn) ret))
	   ret)))
      (EQO
       (let* ((ret nil)
	      (eed1 nil)
	      (eed2 nil)
	      (eed3 nil)
	      (eed4 nil)
	      (edprems1 (car edprems))
	      (edprems2 (cadr edprems))
	      (wffs1 (ext-seq-wffs (car prems)))
	      (wffs2 (ext-seq-wffs (cadr prems)))
	      (pwff (car princ-wffs))
	      (pwff1 (cdr pwff))
	      (lft (cdar pwff1))
	      (rght (cdr pwff1))
	      (nlft (cons 'NOT lft))
	      (nrght (cons 'NOT rght)))
	 (dolist (w wffs1)
	   (let ((a (assoc w edprems1 :test #'wffeq-ab)))
	     (unless a
	       (throwfail "problem building ext-exp-dag"))
	     (setq edprems1 (remove a edprems1 :count 1))
	     (if (and (wffeq-ab w lft) (not eed1))
		 (setq eed1 (cdr a))
	       (if (and (wffeq-ab w rght) (not eed2))
		   (setq eed2 (cdr a))
		 (let ((b (assoc w edprems2 :test #'wffeq-ab)))
		   (unless b
		     (throwfail "problem building ext-exp-dag"))
		   (setq edprems2 (remove b edprems2 :count 1))
		   (push (cons w (ext-exp-dag-pre-merge (cdr a) (cdr b))) ret))))))
	 (let ((b (assoc nlft edprems2 :test #'wffeq-ab)))
	   (setq eed3 (ext-exp-dag-remove-neg (cdr b)))
	   (setq edprems2 (remove b edprems2 :count 1))
	   (setq eed4 (ext-exp-dag-remove-neg (cdr (assoc nrght edprems2 :test #'wffeq-ab)))))
	 (unless (and eed1 eed2 eed3 eed4)
	   (throwfail "problem building ext-exp-dag"))
	 (let ((rewrite-as-conj (member rewrite-equivs '(2 3 4) :test #'equal)))
	   (if rewrite-as-conj
	       (let ((lft2 (ext-seq-to-eed-instantiate-all lft))
		     (rght2 (ext-seq-to-eed-instantiate-all rght)))
		 (multiple-value-bind
		     (eed03 eed1)
		     (make-eed-nonatom-mates lft2)
		   (multiple-value-bind
		       (eed04 eed02)
		       (make-eed-nonatom-mates rght2)
		     (let* ((eed1m (ext-exp-dag-pre-merge eed1 eed01))
			    (eed2m (ext-exp-dag-pre-merge eed2 eed02))
			    (eed3m (ext-exp-dag-pre-merge eed3 eed03))
			    (eed4m (ext-exp-dag-pre-merge eed4 eed04))
			    (equiv-node
			     (make-eed-rew (acons 'EQUIV lft2 rght2)
					   'EQUIV-IMPLICS
					   (make-eed-con
					    (make-eed-imp eed1m eed4m)
					    (make-eed-imp eed2m eed3m)))))
		       (push (cons pwff (make-eed-neg
					 (make-eed-eqo (ext-seq-to-eed-instantiate-all pwff1) equiv-node))) ret)))))
	     (let ((equiv-node
		    (make-eed-rew (acons 'EQUIV
					 (ext-seq-to-eed-instantiate-all lft)
					 (ext-seq-to-eed-instantiate-all rght))
				  'EQUIV-DISJS
				  (make-eed-dis (make-eed-con eed3 eed4)
						(make-eed-con (make-eed-neg eed1) (make-eed-neg eed2))))))
	       (push (cons pwff (make-eed-neg (make-eed-eqo (ext-seq-to-eed-instantiate-all pwff1) equiv-node))) ret))))))
      (EXTO
       (let* ((ret nil)
	      (eed1 nil)
	      (eed2 nil)
	      (eed3 nil)
	      (eed4 nil)
	      (edprems1 (car edprems))
	      (edprems2 (cadr edprems))
	      (wffs1 (ext-seq-wffs (car prems)))
	      (wffs2 (ext-seq-wffs (cadr prems)))
	      (pwff (car princ-wffs))
	      (lft (cdar pwff))
	      (rght (cdr pwff))
	      (nlft (cons 'NOT lft))
	      (nrght (cons 'NOT rght)))
	 (dolist (w wffs1)
	   (let ((a (assoc w edprems1 :test #'wffeq-ab)))
	     (unless a
	       (throwfail "problem building ext-exp-dag"))
	     (setq edprems1 (remove a edprems1 :count 1))
	     (if (and (wffeq-ab w nlft) (not eed1))
		 (setq eed1 (ext-exp-dag-remove-neg (cdr a)))
	       (if (and (wffeq-ab w rght) (not eed2))
		   (setq eed2 (cdr a))
		 (let ((b (assoc w edprems2 :test #'wffeq-ab)))
		   (unless b
		     (throwfail "problem building ext-exp-dag"))
		   (setq edprems2 (remove b edprems2 :count 1))
		   (push (cons w (ext-exp-dag-pre-merge (cdr a) (cdr b))) ret))))))
	 (let ((b (assoc lft edprems2 :test #'wffeq-ab)))
	   (setq eed3 (cdr b))
	   (setq edprems2 (remove b edprems2 :count 1))
	   (setq eed4 (ext-exp-dag-remove-neg (cdr (assoc nrght edprems2 :test #'wffeq-ab)))))
	 (unless (and eed1 eed2 eed3 eed4)
	   (throwfail "problem building ext-exp-dag"))
	 (let* ((lft2 (ext-seq-to-eed-instantiate-all lft))
		(rght2 (ext-seq-to-eed-instantiate-all rght))
		(equivwff (acons 'EQUIV lft2 rght2))
		(rewrite-as-conj (not (member rewrite-equivs '(2 3 5) :test #'equal))))
	   (if rewrite-as-conj
	       (let ((equiv-node
		      (make-eed-rew equivwff
				    'EQUIV-IMPLICS
				    (make-eed-con
				     (make-eed-imp eed1 eed2)
				     (make-eed-imp eed4 eed3)))))
		 (push (cons pwff (make-eed-exto (ext-seq-to-eed-instantiate-all pwff) equiv-node)) ret))
	     (multiple-value-bind
		 (eed01 eed03)
		 (make-eed-nonatom-mates lft2)
	       (multiple-value-bind
		   (eed04 eed02)
		   (make-eed-nonatom-mates rght2)
		 (let* ((eed1m (ext-exp-dag-pre-merge eed1 eed01))
			(eed2m (ext-exp-dag-pre-merge eed2 eed02))
			(eed3m (ext-exp-dag-pre-merge eed3 eed03))
			(eed4m (ext-exp-dag-pre-merge eed4 eed04))
			(equiv-node
			 (make-eed-rew equivwff
				       'EQUIV-DISJS
				       (make-eed-dis
					(make-eed-con eed3m eed2m)
					(make-eed-con (make-eed-neg eed1m)
						      (make-eed-neg eed4m))))))
		   (push (cons pwff (make-eed-exto (ext-seq-to-eed-instantiate-all pwff) equiv-node)) ret))))))))
      (EQFUNC
       (let* ((ret nil)
	      (eed1 nil)
	      (edprems1 (car edprems))
	      (wffs1 (ext-seq-wffs (car prems)))
	      (pwff (car princ-wffs))
	      (pwff1 (cdr pwff))
	      (lft (cdar pwff1))
	      (rght (cdr pwff1))
	      (tp (unabbreviated-type lft))
	      (exp (ext-seq-exp-term es))
	      (q (inherit-abbrev '= (cons (cons 'O (car tp)) (car tp)) (list (car tp))))
	      (x (fresh-var-1 (cdr tp)))
	      (awff (acons x 'FORALL (acons q (cons lft x) (cons rght x))))
	      (pwff2 (acons q (cons lft exp) (cons rght exp)))
	      (pwff3 (cons 'NOT pwff2)))
	 (dolist (w wffs1)
	   (let ((a (assoc w edprems1 :test #'wffeq-ab)))
	     (unless a
	       (throwfail "problem building ext-exp-dag"))
	     (setq edprems1 (remove a edprems1 :count 1))
	     (if (and (wffeq-ab w pwff3) (not eed1))
		 (setq eed1 (ext-exp-dag-remove-neg (cdr a)))
	       (push a ret))))
	 (unless eed1
	   (throwfail "problem building ext-exp-dag"))
	 (let* ((exp00 (ext-seq-to-eed-instantiate-all exp))
		(lft00 (ext-seq-to-eed-instantiate-all lft))
		(rght00 (ext-seq-to-eed-instantiate-all rght))
		(pwff20 (acons q (cons lft00 exp00) (cons rght00 exp00)))
		(awff00 (acons x 'FORALL (acons q (cons lft00 x) (cons rght00 x))))
		(eed3 (make-eed-rew pwff20 'LAMBDA eed1))
		(eed4 (make-eed-exp-1 awff00 exp eed3))
		(eed5 (make-eed-rew (ext-seq-to-eed-instantiate-all pwff1) 'EXT= eed4)))
	   (push (cons pwff (make-eed-neg eed5)) ret)
	   ret)))
      (EXTFUNC
       (let* ((ret nil)
	      (eed1 nil)
	      (edprems1 (car edprems))
	      (wffs1 (ext-seq-wffs (car prems)))
	      (pwff (car princ-wffs))
	      (lft (cdar pwff))
	      (rght (cdr pwff))
	      (tp (unabbreviated-type lft))
	      (sv (ext-seq-sel-var es))
	      (q (inherit-abbrev '= (cons (cons 'O (car tp)) (car tp)) (list (car tp))))
	      (x (fresh-var-1 (cdr tp)))
	      (awff (acons x 'FORALL (acons q (cons lft x) (cons rght x))))
	      (pwff2 (acons q (cons lft sv) (cons rght sv))))
	 (dolist (w wffs1)
	   (let ((a (assoc w edprems1 :test #'wffeq-ab)))
	     (unless a
	       (throwfail "problem building ext-exp-dag"))
	     (setq edprems1 (remove a edprems1 :count 1))
	     (if (and (wffeq-ab w pwff2) (not eed1))
		 (setq eed1 (cdr a))
	       (push a ret))))
	 (unless eed1
	   (throwfail "problem building ext-exp-dag"))
	 (let* ((lft00 (ext-seq-to-eed-instantiate-all lft))
		(rght00 (ext-seq-to-eed-instantiate-all rght))
		(pwff20 (acons q (cons lft00 sv) (cons rght00 sv)))
		(awff00 (acons x 'FORALL (acons q (cons lft00 x) (cons rght00 x))))
		(eed3 (make-eed-rew pwff20 'LAMBDA eed1))
		(eed4 (make-eed-sel-1 awff00 sv eed3))
		(eed5 (make-eed-rew (ext-seq-to-eed-instantiate-all pwff) 'EXT= eed4)))
	   (push (cons pwff eed5) ret)
	   ret)))
      (REW
       (let* ((ret nil)
	      (eed1 nil)
	      (edprems1 (car edprems))
	      (wffs1 (ext-seq-wffs (car prems)))
	      (pwff (car princ-wffs))
	      (gam (multiset-extract-wffs wffs pwff)))
	 (dolist (w wffs1)
	   (let ((a (assoc w edprems1 :test #'wffeq-ab)))
	     (unless a
	       (throwfail "problem building ext-exp-dag"))
	     (setq edprems1 (remove a edprems1 :count 1))
	     (if (and (not (member w gam :test #'wffeq-ab)) (not eed1))
		 (setq eed1 (cdr a))
	       (push a ret))))
	 (unless eed1
	   (throwfail "problem building ext-exp-dag"))
	 (push (cons pwff eed1) ret)
	 ret))
      (CONTR 
       (let* ((ret nil)
	      (eed1 nil)
	      (eed2 nil)
	      (edprems1 (car edprems))
	      (wffs1 (ext-seq-wffs (car prems)))
	      (pwff (car princ-wffs)))
	 (dolist (w wffs1)
	   (let ((a (assoc w edprems1 :test #'wffeq-ab)))
	     (unless a
	       (throwfail "problem building ext-exp-dag"))
	     (setq edprems1 (remove a edprems1 :count 1))
	     (if (and (wffeq-ab w pwff) (not eed1))
		 (setq eed1 (cdr a))
	       (if (and (wffeq-ab w pwff) (not eed2))
		   (setq eed2 (cdr a))
		 (push a ret)))))
	 (unless (and eed1 eed2)
	   (throwfail "problem building ext-exp-dag"))
	 (push (cons pwff (ext-exp-dag-pre-merge eed1 eed2)) ret)
	 ret))
      (CUT (throwfail "Cannot handle Cut"))
      (INIT
       (cutfree-ext-seq-to-ext-exp-dags-1
	(ext-seq-expand-init (cdar princ-wffs) (cadr princ-wffs)
			     (multiset-extract-wffs wffs (car princ-wffs) (cadr princ-wffs)))))
      (REFL
       (cutfree-ext-seq-to-ext-exp-dags-1
	(ext-seq-expand-refl (cdaar princ-wffs) (cdar princ-wffs)
			     (multiset-extract-wffs wffs (car princ-wffs))))))))

; pre-merge assumes no EQUIVWFFS rewrites.
(defun ext-exp-dag-pre-merge-list (eed1 eedl)
  (if eedl
      (ext-exp-dag-pre-merge-list
       (ext-exp-dag-pre-merge eed1 (car eedl))
       (cdr eedl))
    eed1))

(defun ext-exp-dag-pre-merge (eed1 eed2)
  (let ((nodes-done nil))
    (declare (special nodes-done))
    (let ((sh1 (ext-exp-dag-shallow eed1))
	  (sh2 (ext-exp-dag-shallow eed2)))
      (unless (wffeq-ab sh1 sh2)
	(setf (get 'ext-exp-dag-verbose 'eed1) eed1)
	(setf (get 'ext-exp-dag-verbose 'eed2) eed2)
	(throwfail "Cannot pre-merge - shallows do not match" t (sh1 . gwff) t (sh2 . gwff)))
      (ext-exp-dag-pre-merge-1 sh1 eed1 eed2))))

; in general, the shallow formulas of eed1 and eed2 should reduce to sh via lambda + defns
(defun ext-exp-dag-pre-merge-1 (sh eed1 eed2)
  (declare (special nodes-done))
  (let ((a (assoc (cons eed1 eed2) nodes-done :test #'equal)))
    (if a
	(cdr a)
      (let ((eed3 (ext-exp-dag-pre-merge-2 sh eed1 eed2)))
	(push (cons (cons eed1 eed2) eed3) nodes-done)
	eed3))))

(defun ext-exp-dag-pre-merge-2 (sh eed1 eed2)
  (if (and (eq (ext-exp-dag-kind eed1) 'REW)
	   (eq (ext-exp-dag-rew-just eed1) 'LAMBDA))
      (ext-exp-dag-pre-merge-2 (etanorm (lambda-norm sh))
			       (ext-exp-arc-node (car (ext-exp-dag-arcs eed1)))
			       eed2)
    (if (eq (ext-exp-dag-kind eed1) 'LEAF)
	eed2
      (ext-exp-dag-pre-merge-3 sh eed1 eed2))))

(defun ext-exp-dag-pre-merge-3 (sh eed1 eed2)
  (if (and (eq (ext-exp-dag-kind eed2) 'REW)
	   (eq (ext-exp-dag-rew-just eed2) 'LAMBDA))
      (ext-exp-dag-pre-merge-3 (etanorm (lambda-norm sh))
			       eed1
			       (ext-exp-arc-node (car (ext-exp-dag-arcs eed2))))
    (if (eq (ext-exp-dag-kind eed2) 'LEAF)
	eed1
      (ext-exp-dag-pre-merge-4 sh eed1 eed2))))

(defun ext-exp-dag-pre-merge-4 (sh eed1 eed2)
  (let* ((k1 (ext-exp-dag-kind eed1))
	 (k2 (ext-exp-dag-kind eed2)))
    (if (and (eq k1 k2)
	     (or (not (eq k1 'REW)) (eq (ext-exp-dag-rew-just eed1)
					(ext-exp-dag-rew-just eed2))))
	(case k1
	  ((ATOM EQN EXP SEL)
	   (let ((eed3 (copy-eed eed1)))
	     (setf (ext-exp-dag-arcs eed3)
		   (append (ext-exp-dag-arcs eed1) (ext-exp-dag-arcs eed2)))
	     eed3))
	  (DEC
	   (let* ((arcs1 (ext-exp-dag-arcs eed1))
		  (arcs2 (ext-exp-dag-arcs eed2))
		  (arcs3 nil)
		  (eed3 (copy-eed eed1)))
	     (dolist (arc1 arcs1)
	       (let* ((i (ext-exp-arc-dec-index arc1))
		      (arc2 (find-if #'(lambda (arc)
					 (equal i (ext-exp-arc-dec-index arc)))
				     arcs2)))
		 (unless arc2
		   (throwfail "pre-merge problem - dec " i " missing from " eed2))
		 (let ((arc3 (ext-exp-arc-pre-merge-1 sh arc1 arc2)))
		   (push arc3 arcs3))))
	     (setf (ext-exp-dag-arcs eed3) arcs3)
	     eed3))
	  ((TRUE FALSE) eed1)
	  (LEAF (make-eed-leaf sh (ext-exp-dag-positive eed1)))
	  (REW
	   (let* ((arcs1 (ext-exp-dag-arcs eed1))
		  (arcs2 (ext-exp-dag-arcs eed2))
		  (sh1 (ext-exp-dag-shallow (ext-exp-arc-node (car arcs1))))
		  (sh2 (ext-exp-dag-shallow (ext-exp-arc-node (car arcs2)))))
	     (unless (wffeq-ab sh1 sh2)
	       (throwfail "Cannot pre-merge rewrites - shallows do not match"))
	     (let ((arc3 (ext-exp-arc-pre-merge-1 sh1 (car arcs1) (car arcs2)))
		   (eed3 (copy-eed eed1)))
	       (setf (ext-exp-dag-arcs eed3) (list arc3))
	       eed3)))
	  (NEG
	   (let* ((sh1 (cdr sh))
		  (arcs1 (ext-exp-dag-arcs eed1))
		  (arcs2 (ext-exp-dag-arcs eed2))
		  (arc3 (ext-exp-arc-pre-merge-1 sh1 (car arcs1) (car arcs2)))
		  (eed3 (copy-eed eed1)))
	     (setf (ext-exp-dag-arcs eed3) (list arc3))
	     eed3))
	  ((DIS CON IMP)
	   (let* ((sh1 (cdar sh))
		  (sh2 (cdr sh))
		  (arcs1 (ext-exp-dag-arcs eed1))
		  (arcs2 (ext-exp-dag-arcs eed2))
		  (arc3 (ext-exp-arc-pre-merge-1 sh1 (car arcs1) (car arcs2)))
		  (arc4 (ext-exp-arc-pre-merge-1 sh2 (cadr arcs1) (cadr arcs2)))
		  (eed3 (copy-eed eed1)))
	     (setf (ext-exp-dag-arcs eed3) (list arc3 arc4))
	     eed3)))
      (throwfail "Merge Error - nodes do not match"))))

(defun ext-exp-arc-pre-merge-1 (sh arc1 arc2)
  (let ((arc3 (copy-ext-exp-arc arc1))
	(es3 (ext-exp-dag-pre-merge-1 sh
				      (ext-exp-arc-node arc1)
				      (ext-exp-arc-node arc2))))
      (setf (ext-exp-arc-node arc3) es3)
      arc3))

(defun ext-exp-dag-conflate-params (theta eed)
  (let ((nodes-done nil))
    (declare (special nodes-done))
    (ext-exp-dag-conflate-params-1 theta eed)))

(defun ext-exp-dag-conflate-params-1 (theta eed)
  (declare (special nodes-done))
  (let ((a (assoc eed nodes-done)))
    (if a
	(cdr a)
      (let* ((sh (ext-exp-dag-shallow eed))
	     (sh2 (simul-substitute-l-term-var theta sh))
	     (arcs (ext-exp-dag-arcs eed))
	     (arcs2 (mapcar #'(lambda (arc) (ext-exp-arc-conflate-params-1 theta arc)) arcs)))
	(if (and (wffeq-ab sh sh2) (equal arcs arcs2))
	    (progn
	      (push (cons eed eed) nodes-done)
	      eed)
	  (let ((eed2 (copy-eed eed)))
	    (setf (ext-exp-dag-shallow eed2) sh2)
	    (setf (ext-exp-dag-arcs eed2) arcs2)
	    (push (cons eed eed2) nodes-done)
	    eed2))))))

(defun ext-exp-arc-conflate-params-1 (theta arc)
  (let* ((node (ext-exp-arc-node arc))
	 (node2 (ext-exp-dag-conflate-params-1 theta node))
	 (expterm (ext-exp-arc-exp-term arc))
	 (expterm2 (when (eq (ext-exp-arc-kind arc) 'EXP)
		     (simul-substitute-l-term-var theta expterm))))
    (if (and (eq node node2) 
	     (or (not (eq (ext-exp-arc-kind arc) 'EXP))
		 (wffeq-ab expterm expterm2)))
	arc
      (let ((arc2 (copy-ext-exp-arc arc)))
	(setf (ext-exp-arc-exp-term arc2) expterm2)
	(setf (ext-exp-arc-node arc2) node2)
	arc2))))

; merge assumes no EQUIVWFFS rewrites.
(defun ext-exp-dag-merge (eed)
  (car (ext-exp-dag-merge-list (list eed))))

(defun ext-exp-dag-merge-list (eedl)
  (do ((merge-red (ext-exp-dag-merge-redex eedl) (ext-exp-dag-merge-redex eedl)))
      ((null merge-red) eedl)
    (case (car merge-red)
      (SEL
       (setq eedl (ext-exp-dag-merge-sel-l eedl (cadr merge-red) (caddr merge-red))))
      (EXP
       (setq eedl (ext-exp-dag-merge-exp-l eedl (cadr merge-red) (caddr merge-red))))
      (MATE
       (setq eedl (ext-exp-dag-merge-mate-l eedl (cadr merge-red) (caddr merge-red)
					    (nth 3 merge-red) (nth 4 merge-red))))
      (DEC
       (setq eedl (ext-exp-dag-merge-dec-l eedl (cadr merge-red) (caddr merge-red)
					   (nth 3 merge-red))))
      (EUNIF1
       (setq eedl (ext-exp-dag-merge-eunif1-l eedl (cadr merge-red) (caddr merge-red)
					      (nth 3 merge-red) (nth 4 merge-red))))
      (EUNIF2
       (setq eedl (ext-exp-dag-merge-eunif2-l eedl (cadr merge-red) (caddr merge-red)
					      (nth 3 merge-red) (nth 4 merge-red)))))
    (when ext-exp-dag-debug
      (unless (ext-exp-dag-check-pf-p eedl :rewrite-defns-eager T)
	(setf (get 'ext-exp-dag-debug 'edags) eedl)
	(setf (get 'ext-exp-dag-debug 'merge-red) merge-red)
	(throwfail "Bug in ext-exp-dag-merge")))))

(defun ext-exp-dag-merge-sel (eed selnode svars)
  (car (ext-exp-dag-merge-sel-l (list eed) selnode svars)))

(defun ext-exp-dag-merge-sel-l (eedl selnode svars)
  (let* ((x (car svars))
	 (theta (mapcar #'(lambda (y) (cons y x)) (cdr svars)))
	 (nodes-done nil)
	 (eedl2 nil))
    (declare (special nodes-done))
    (dolist (eed eedl)
      (push (ext-exp-dag-merge-sel-1 eed selnode x theta) eedl2))
    (reverse eedl2)))

(defun ext-exp-dag-merge-sel-1 (eed selnode x theta)
  (declare (special nodes-done))
  (let ((a (assoc eed nodes-done)))
    (if a
	(cdr a)
      (let ((arcs (ext-exp-dag-arcs eed))
	    (sh (ext-exp-dag-shallow eed)))
	(if (eq eed selnode)
	    (let* ((nodes (mapcar #'(lambda (arc)
				      (ext-exp-dag-conflate-params-1
				       theta (ext-exp-arc-node arc)))
				  arcs))
		   (node2 (ext-exp-dag-pre-merge-list (car nodes) (cdr nodes)))
		   (eed2 (make-eed-sel-1 sh x node2)))
		(push (cons eed eed2) nodes-done)
		eed2)
	  (let ((sh2 (simul-substitute-l-term-var theta sh))
		(arcs2 (mapcar #'(lambda (arc)
				   (ext-exp-arc-merge-sel-1 arc selnode x theta))
			       arcs)))
	    (if (and (wffeq-ab sh sh2) (equal arcs arcs2))
		(progn
		  (push (cons eed eed) nodes-done)
		  eed)
	      (let ((eed2 (copy-eed eed)))
		(setf (ext-exp-dag-shallow eed2) sh2)
		(setf (ext-exp-dag-arcs eed2) arcs2)
		(push (cons eed eed2) nodes-done)
		eed2))))))))

(defun ext-exp-arc-merge-sel-1 (arc selnode x theta)
  (let* ((node (ext-exp-arc-node arc))
	 (node2 (ext-exp-dag-merge-sel-1 node selnode x theta))
	 (expterm (ext-exp-arc-exp-term arc))
	 (expterm2 (when (eq (ext-exp-arc-kind arc) 'EXP)
		     (simul-substitute-l-term-var theta expterm))))
    (if (and (eq node node2) 
	     (or (not (eq (ext-exp-arc-kind arc) 'EXP))
		 (wffeq-ab expterm expterm2)))
	arc
      (let ((arc2 (copy-ext-exp-arc arc)))
	(setf (ext-exp-arc-exp-term arc2) expterm2)
	(setf (ext-exp-arc-node arc2) node2)
	arc2))))

(defun ext-exp-dag-merge-exp (eed expnode dupterm)
  (car (ext-exp-dag-merge-exp-l (list eed) expnode dupterm)))

(defun ext-exp-dag-merge-exp-l (eedl expnode dupterm)
  (let ((nodes-done nil)
	(eedl2 nil))
    (declare (special nodes-done))
    (dolist (eed eedl)
      (push (ext-exp-dag-merge-exp-1 eed expnode dupterm) eedl2))
    (reverse eedl2)))

(defun ext-exp-dag-merge-exp-1 (eed expnode dupterm)
  (declare (special nodes-done))
  (let ((a (assoc eed nodes-done)))
    (if a
	(cdr a)
      (let ((arcs (ext-exp-dag-arcs eed)))
	(if (eq eed expnode)
	    (let ((arcs2 nil)
		  (merge-nodes nil))
	      (dolist (arc arcs)
		(if (wffeq-ab (ext-exp-arc-exp-term arc) dupterm)
		    (push (ext-exp-arc-node arc) merge-nodes)
		  (push arc arcs2)))
	      (let ((eed2 (make-eea-exp (ext-exp-dag-shallow eed)
					(ext-exp-dag-positive eed)
					(cons (make-ext-exp-arc :kind 'EXP
								:exp-term dupterm
								:node (ext-exp-dag-pre-merge-list
								       (car merge-nodes) (cdr merge-nodes)))
					      arcs2))))
		(push (cons eed eed2) nodes-done)
		eed2))
	  (let ((arcs2 (mapcar #'(lambda (x)
				   (ext-exp-arc-merge-exp-1 x expnode dupterm))
			       arcs)))
	    (if (equal arcs arcs2)
		(progn
		  (push (cons eed eed) nodes-done)
		  eed)
	      (let ((eed2 (copy-eed eed)))
		(setf (ext-exp-dag-arcs eed2) arcs2)
		(push (cons eed eed2) nodes-done)
		eed2))))))))

(defun ext-exp-arc-merge-exp-1 (arc expnode dupterm)
  (let* ((node (ext-exp-arc-node arc))
	 (node2 (ext-exp-dag-merge-exp-1 node expnode dupterm)))
    (if (eq node node2)
	arc
      (let ((arc2 (copy-ext-exp-arc arc)))
	(setf (ext-exp-arc-node arc2) node2)
	arc2))))

(defun ext-exp-dag-merge-mate (eed posnode negnode eqn1 eqn2)
  (car (ext-exp-dag-merge-mate-l (list eed) posnode negnode eqn1 eqn2)))

(defun ext-exp-dag-merge-mate-l (eedl posnode negnode eqn1 eqn2)
  (let ((eqn (ext-exp-dag-pre-merge eqn1 eqn2))
	(nodes-done nil)
	(eedl2 nil))
    (declare (special nodes-done))
    (dolist (eed eedl)
      (push (ext-exp-dag-merge-mate-1 eed (list posnode negnode) (list eqn1 eqn2) eqn)
	    eedl2))
    (reverse eedl2)))

(defun ext-exp-dag-merge-mate-1 (eed oldnodes oldeqns eqn)
  (declare (special nodes-done))
  (let ((a (assoc eed nodes-done)))
    (if a
	(cdr a)
      (let ((arcs (ext-exp-dag-arcs eed)))
	(if (member eed oldnodes)
	    (let ((arcs2 (cons (make-ext-exp-arc :kind 'MATE :node eqn)
			       (remove-if #'(lambda (arc) (member (ext-exp-arc-node arc) oldeqns))
					  arcs)))
		  (eed2 (copy-eed eed)))
	      (setf (ext-exp-dag-arcs eed2) arcs2)
	      (push (cons eed eed2) nodes-done)
	      eed2)
	  (let ((arcs2 (mapcar #'(lambda (x)
				   (ext-exp-arc-merge-mate-1 x oldnodes oldeqns eqn))
			       arcs)))
	    (if (equal arcs arcs2)
		(progn
		  (push (cons eed eed) nodes-done)
		  eed)
	      (let ((eed2 (copy-eed eed)))
		(setf (ext-exp-dag-arcs eed2) arcs2)
		(push (cons eed eed2) nodes-done)
		eed2))))))))

(defun ext-exp-arc-merge-mate-1 (arc oldnodes oldeqns eqn)
  (let* ((node (ext-exp-arc-node arc))
	 (node2 (ext-exp-dag-merge-mate-1 node oldnodes oldeqns eqn)))
    (if (eq node node2)
	arc
      (let ((arc2 (copy-ext-exp-arc arc)))
	(setf (ext-exp-arc-node arc2) node2)
	arc2))))

(defun ext-exp-dag-merge-dec (eed eqnnode arc1 arc2)
  (car (ext-exp-dag-merge-dec-l (list eed) eqnnode arc1 arc2)))

(defun ext-exp-dag-merge-dec-l (eedl eqnnode arc1 arc2)
  (let ((arc3 (copy-ext-exp-arc arc1))
	(nodes-done nil)
	(eedl2 nil))
    (declare (special nodes-done))
    (setf (ext-exp-arc-node arc3)
	  (ext-exp-dag-pre-merge (ext-exp-arc-node arc1) (ext-exp-arc-node arc2)))
    (dolist (eed eedl)
      (push (ext-exp-dag-merge-dec-1 eed eqnnode (list arc1 arc2) arc3)
	    eedl2))
    (reverse eedl2)))

(defun ext-exp-dag-merge-dec-1 (eed eqnnode oldarcs arc3)
  (declare (special nodes-done))
  (let ((a (assoc eed nodes-done)))
    (if a
	(cdr a)
      (let ((arcs (ext-exp-dag-arcs eed)))
	(if (eq eed eqnnode)
	    (let ((arcs2 (cons arc3
			       (remove-if #'(lambda (arc) (member arc oldarcs))
					  arcs)))
		  (eed2 (copy-eed eed)))
	      (setf (ext-exp-dag-arcs eed2) arcs2)
	      (push (cons eed eed2) nodes-done)
	      eed2)
	  (let ((arcs2 (mapcar #'(lambda (x)
				   (ext-exp-arc-merge-dec-1 x eqnnode oldarcs arc3))
			       arcs)))
	    (if (equal arcs arcs2)
		(progn
		  (push (cons eed eed) nodes-done)
		  eed)
	      (let ((eed2 (copy-eed eed)))
		(setf (ext-exp-dag-arcs eed2) arcs2)
		(push (cons eed eed2) nodes-done)
		eed2))))))))

(defun ext-exp-arc-merge-dec-1 (arc eqnnode oldarcs arc3)
  (let* ((node (ext-exp-arc-node arc))
	 (node2 (ext-exp-dag-merge-dec-1 node eqnnode oldarcs arc3)))
    (if (eq node node2)
	arc
      (let ((arc2 (copy-ext-exp-arc arc)))
	(setf (ext-exp-arc-node arc2) node2)
	arc2))))

(defun ext-exp-dag-merge-eunif1 (eed poseqn negeqn eu1 eu2)
  (car (ext-exp-dag-merge-eunif1-l (list eed) poseqn negeqn eu1 eu2)))

(defun ext-exp-dag-merge-eunif1-l (eedl poseqn negeqn eu1 eu2)
  (let ((eu3 (ext-exp-dag-pre-merge eu1 eu2))
	(nodes-done nil)
	(eedl2 nil))
    (declare (special nodes-done))
    (dolist (eed eedl)
      (push (ext-exp-dag-merge-eunif1-1 eed (list poseqn negeqn) (list eu1 eu2) eu3)
	    eedl2))
    (reverse eedl2)))

(defun ext-exp-dag-merge-eunif1-1 (eed oldnodes oldeus eu)
  (declare (special nodes-done))
  (let ((a (assoc eed nodes-done)))
    (if a
	(cdr a)
      (let ((arcs (ext-exp-dag-arcs eed)))
	(if (member eed oldnodes)
	    (let ((arcs2 (cons (make-ext-exp-arc :kind 'EUNIF1 :node eu)
			       (remove-if #'(lambda (arc) (member (ext-exp-arc-node arc) oldeus))
					  arcs)))
		  (eed2 (copy-eed eed)))
	      (setf (ext-exp-dag-arcs eed2) arcs2)
	      (push (cons eed eed2) nodes-done)
	      eed2)
	  (let ((arcs2 (mapcar #'(lambda (x)
				   (ext-exp-arc-merge-eunif1-1 x oldnodes oldeus eu))
			       arcs)))
	    (if (equal arcs arcs2)
		(progn
		  (push (cons eed eed) nodes-done)
		  eed)
	      (let ((eed2 (copy-eed eed)))
		(setf (ext-exp-dag-arcs eed2) arcs2)
		(push (cons eed eed2) nodes-done)
		eed2))))))))

(defun ext-exp-arc-merge-eunif1-1 (arc oldnodes oldeus eu)
  (let* ((node (ext-exp-arc-node arc))
	 (node2 (ext-exp-dag-merge-eunif1-1 node oldnodes oldeus eu)))
    (if (eq node node2)
	arc
      (let ((arc2 (copy-ext-exp-arc arc)))
	(setf (ext-exp-arc-node arc2) node2)
	arc2))))

(defun ext-exp-dag-merge-eunif2 (eed poseqn negeqn eu1 eu2)
  (car (ext-exp-dag-merge-eunif2-l (list eed) poseqn negeqn eu1 eu2)))

(defun ext-exp-dag-merge-eunif2-l (eedl poseqn negeqn eu1 eu2)
  (let ((eu3 (ext-exp-dag-pre-merge eu1 eu2))
	(nodes-done nil)
	(eedl2 nil))
    (declare (special nodes-done))
    (dolist (eed eedl)
      (push (ext-exp-dag-merge-eunif2-1 eed (list poseqn negeqn) (list eu1 eu2) eu3)
	    eedl2))
    (reverse eedl2)))

(defun ext-exp-dag-merge-eunif2-1 (eed oldnodes oldeus eu)
  (declare (special nodes-done))
  (let ((a (assoc eed nodes-done)))
    (if a
	(cdr a)
      (let ((arcs (ext-exp-dag-arcs eed)))
	(if (member eed oldnodes)
	    (let ((arcs2 (cons (make-ext-exp-arc :kind 'EUNIF2 :node eu)
			       (remove-if #'(lambda (arc) (member (ext-exp-arc-node arc) oldeus))
					  arcs)))
		  (eed2 (copy-eed eed)))
	      (setf (ext-exp-dag-arcs eed2) arcs2)
	      (push (cons eed eed2) nodes-done)
	      eed2)
	  (let ((arcs2 (mapcar #'(lambda (x)
				   (ext-exp-arc-merge-eunif2-1 x oldnodes oldeus eu))
			       arcs)))
	    (if (equal arcs arcs2)
		(progn
		  (push (cons eed eed) nodes-done)
		  eed)
	      (let ((eed2 (copy-eed eed)))
		(setf (ext-exp-dag-arcs eed2) arcs2)
		(push (cons eed eed2) nodes-done)
		eed2))))))))

(defun ext-exp-arc-merge-eunif2-1 (arc oldnodes oldeus eu)
  (let* ((node (ext-exp-arc-node arc))
	 (node2 (ext-exp-dag-merge-eunif2-1 node oldnodes oldeus eu)))
    (if (eq node node2)
	arc
      (let ((arc2 (copy-ext-exp-arc arc)))
	(setf (ext-exp-arc-node arc2) node2)
	arc2))))

(defun ext-exp-dag-merge-redex (eedl)
  (let ((mates nil)
	(mates2 nil)
	(eunifs1 nil)
	(eunifs2 nil)
	(eunifs1p nil)
	(eunifs2p nil)
	(nodes-done nil)
	(ret nil))
    (declare (special mates mates2 eunifs1 eunifs1p eunifs2 eunifs2p nodes-done))
    (do ((eedl1 eedl (cdr eedl1)))
	((or ret (null eedl1)) ret)
      (setq ret (ext-exp-dag-merge-redex-1 (car eedl1))))))

(defun ext-exp-dag-merge-redex-1 (eed)
  (declare (special mates mates2 eunifs1 eunifs1p eunifs2 eunifs2p))
  (unless (member eed nodes-done)
    (push eed nodes-done)
    (let ((arcs (ext-exp-dag-arcs eed))
	  (ret nil))
      (do ((arcs1 arcs (cdr arcs1)))
	  ((or ret (null arcs1)))
	(setq ret (ext-exp-dag-merge-redex-1 (ext-exp-arc-node (car arcs1)))))
      (if ret
	  ret
	(let* ((k (ext-exp-dag-kind eed))
	       (pos (ext-exp-dag-positive eed))
	       (expterms nil)
	       (dupterm nil)
	       (node2 nil)
	       (dupdec nil)
	       (dupdec2 nil)
	       (dupmate nil)
	       (dupmate2 nil)
	       (eu nil)
	       (dupeunif nil)
	       (dupeunif2 nil))
	  (when (eq k 'EXP)
	    (setq expterms (mapcar #'(lambda (x)
				       (ext-exp-arc-exp-term x))
				   arcs))
	    (do ((expterms2 expterms (cdr expterms2)))
		((or dupterm (null expterms2)))
	      (when (member (car expterms2) (cdr expterms2) :test #'wffeq-ab)
		(setq dupterm (car expterms2)))))
	  (when (eq k 'ATOM)
	    (dolist (arc arcs)
	      (let* ((m (ext-exp-arc-node arc))
		     (a (assoc m mates)))
		(if a
		    (let ((b (assoc (cons eed (cdr a)) mates2 :test #'equal)))
		      (when (and b (not (eq m (cdr b))))
			(setq dupmate m dupmate2 (cdr b) node2 (cdr a)))
		      (push (acons eed (cdr a) m) mates2))
		  (push (cons m eed) mates)))))
	  (when (eq k 'EQN)
	    (dolist (arc arcs)
	      (let ((ak (ext-exp-arc-kind arc)))
		(case ak
		  (EQNDEC
		   (if dupdec
		       (unless dupdec2
			 (setq dupdec2 arc))
		     (setq dupdec arc)))
		  (EUNIF1
		   (let* ((m (ext-exp-arc-node arc))
			  (a (assoc m eunifs1)))
		     (if a
			 (let ((b (assoc (cons eed (cdr a)) eunifs1p :test #'equal)))
			   (when (and b (not (eq m (cdr b))))
			     (setq eu 'EUNIF1 node2 (cdr a) dupeunif m dupeunif2 (cdr b)))
			   (push (cons eed (cdr a)) eunifs1p))
		       (push (cons m eed) eunifs1))))
		  (EUNIF2
		   (let* ((m (ext-exp-arc-node arc))
			  (a (assoc m eunifs2)))
		     (if a
			 (let ((b (assoc (cons eed (cdr a)) eunifs2p :test #'equal)))
			   (when (and b (not (eq m (cdr b))))
			     (setq eu 'EUNIF2 node2 (cdr a) dupeunif m dupeunif2 (cdr b)))
			   (push (cons eed (cdr a)) eunifs2p))
		       (push (cons m eed) eunifs2))))))))
	  (cond (dupterm (list 'EXP eed dupterm))
		(dupdec2 (list 'DEC eed dupdec dupdec2))
		(dupmate
		 (if pos
		     (list 'MATE eed node2 dupmate dupmate2)
		   (list 'MATE node2 eed dupmate dupmate2)))
		(eu
		 (if pos
		     (list eu eed node2 dupeunif dupeunif2)
		   (list eu node2 eed dupeunif dupeunif2)))
		((and (eq k 'SEL) (> (length arcs) 1))
		 (list 'SEL eed (mapcar #'(lambda (x)
					    (ext-exp-arc-sel-var x))
					arcs)))
		(t nil)))))))

(defun ext-exp-dag-sel-vars (eed)
  (let ((nodes-checked nil))
    (declare (special nodes-checked))
    (remove-duplicates (ext-exp-dag-sel-vars-1 eed))))
    
(defun ext-exp-dag-sel-vars-1 (eed)
  (declare (special nodes-checked))
  (if (member eed nodes-checked)
      nil
    (let ((selvars nil)
	  (k (ext-exp-dag-kind eed)))
      (push eed nodes-checked)
      (dolist (arc (ext-exp-dag-arcs eed))
	(setq selvars (append (ext-exp-dag-sel-vars-1 (ext-exp-arc-node arc))
			      selvars))
	(when (eq (ext-exp-dag-kind eed) 'SEL)
	  (push (ext-exp-arc-sel-var arc) selvars)))
      selvars)))

(defun ext-exp-dag-dep-reln (eed)
  (let ((selvars (ext-exp-dag-sel-vars eed))
	(nodes-delayed nil)
	(dep-reln nil)
	(emb-reln nil))
    (declare (special nodes-delayed dep-reln emb-reln))
    (ext-exp-dag-dep-reln-1 eed selvars)
    (values dep-reln emb-reln)))

(defun ext-exp-dag-dep-reln-1 (eed selvars &optional expsabove)
  (declare (special nodes-delayed dep-reln emb-reln))
  (let ((k (ext-exp-dag-kind eed)))
    (case k
      (ATOM
       (dolist (arc (ext-exp-dag-arcs eed))
	 (let* ((n (ext-exp-arc-node arc))
		(a (assoc n nodes-delayed)))
	   (if a
	       (ext-exp-dag-dep-reln-1 n selvars (union (cdr a) expsabove))
	     (push (cons n expsabove) nodes-delayed)))))
      ((EQN DEC)
       (dolist (arc (ext-exp-dag-arcs eed))
	 (let ((n (ext-exp-arc-node arc)))
	   (if (eq (ext-exp-arc-kind arc) 'DEC)
	       (ext-exp-dag-dep-reln-1 n selvars expsabove)
	     (let ((a (assoc n nodes-delayed)))
	       (if a
		   (ext-exp-dag-dep-reln-1 n selvars (union (cdr a) expsabove))
		 (push (cons n expsabove) nodes-delayed)))))))
      (EXP
       (dolist (arc (ext-exp-dag-arcs eed))
	 (let ((frees (intersection (free-vars-of (ext-exp-arc-exp-term arc))
				    selvars)))
	   (push (cons arc frees) dep-reln)
	   (ext-exp-dag-dep-reln-1
	    (ext-exp-arc-node arc) selvars (cons arc expsabove)))))
      (SEL
       (dolist (arc (ext-exp-dag-arcs eed))
	 (push (cons (ext-exp-arc-sel-var arc) expsabove) emb-reln)
	 (ext-exp-dag-dep-reln-1
	  (ext-exp-arc-node arc) selvars expsabove)))
      (t
       (dolist (arc (ext-exp-dag-arcs eed))
	 (ext-exp-dag-dep-reln-1
	  (ext-exp-arc-node arc) selvars expsabove))))))

(defun ext-exp-dag-sel-acyclicity-p (eed)
  (multiple-value-bind
      (dep-reln emb-reln)
      (ext-exp-dag-dep-reln eed)
    (let ((sel-vars (mapcar #'car emb-reln))
	  (svars-checked nil)
	  (exparcs-checked nil)
	  (cycle nil))
      (declare (special svars-checked exparcs-checked))
      (do ((vars sel-vars (cdr vars)))
	  ((or cycle (null vars)) (not cycle))
	(setq cycle (ext-exp-dag-dep-cycle (car vars) emb-reln dep-reln))))))

(defun ext-exp-dag-dep-cycle (sv emb-reln dep-reln &optional svars exparcs)
  (declare (special svars-checked))
  (if (member sv svars-checked)
      nil
    (let ((cycle nil)
	  (svars2 (cons sv svars)))
      (do ((arcs (cdr (assoc sv emb-reln)) (cdr arcs)))
	  ((or (null arcs) cycle)
	   (push sv svars-checked)
	   cycle)
	(setq cycle
	      (or (member (car arcs) exparcs)
		  (ext-exp-dag-dep-cycle-exp-arc (car arcs)
						 emb-reln dep-reln svars2 exparcs)))))))


(defun ext-exp-dag-dep-cycle-exp-arc (arc emb-reln dep-reln &optional svars exparcs)
  (declare (special exparcs-checked))
  (if (member arc exparcs-checked)
      nil
    (let ((cycle nil)
	  (exparcs2 (cons arc exparcs)))
      (do ((svl (cdr (assoc arc dep-reln)) (cdr svl)))
	  ((or (null svl) cycle)
	   (push arc exparcs-checked)
	   cycle)
	(setq cycle
	      (or (member (car svl) svars)
		  (ext-exp-dag-dep-cycle (car svl)
					 emb-reln dep-reln svars exparcs2)))))))

; check if there are no "vertical" paths
; (without computing the jform here)
(defun ext-exp-dag-complete (eed &optional banned blocked-nodes)
  (let ((open-path nil))
    (declare (special open-path))
    (ext-exp-dag-complete-conjs (list eed) banned blocked-nodes)
    (if open-path
	(values nil open-path)
      t)))

(defun ext-exp-dag-complete-l (eedl &optional banned blocked-nodes)
  (if eedl
      (let ((open-path nil))
	(declare (special open-path))
	(ext-exp-dag-complete-conjs eedl banned blocked-nodes)
	(if open-path
	    (values nil open-path)
	  t))
    (values nil nil)))

(defun ext-exp-dag-complete-conjs (eedl &optional banned blocked-nodes)
  (declare (special open-path))
  (let* ((acc (ext-exp-dag-accessible-kids eedl banned blocked-nodes))
	 (done (assoc 'DONE acc))
	 (exp (assoc 'EXP acc))
	 (sel (assoc 'SEL acc))
	 (conj (assoc 'CONJ acc))
	 (disj (assoc 'DISJ acc))
	 (eunif (assoc 'EUNIF acc))
	 (mate (assoc 'MATE acc)))
    (cond (done t)
	  (eunif
	   (ext-exp-dag-complete-conjs
	    (cons (cadddr eunif) eedl)
	    banned (cons (cadddr eunif) blocked-nodes)))
	  (mate
	   (ext-exp-dag-complete-conjs
	    (cons (cadddr mate) eedl)
	    banned (cons (cadddr mate) blocked-nodes)))
	  (sel
	   (let ((sv (ext-exp-arc-sel-var (caddr sel)))
		 (selkid (ext-exp-arc-node (caddr sel))))
	     (ext-exp-dag-complete-conjs
	      (cons selkid eedl)
	      (remove sv banned)
	      (cons selkid blocked-nodes))))
	  (exp
	   (let ((expkid (ext-exp-arc-node (caddr exp))))
	     (ext-exp-dag-complete-conjs (cons expkid eedl) banned
					 (cons expkid blocked-nodes))))
	  (conj
	   (ext-exp-dag-complete-conjs
	    (append (caddr conj) (remove (cadr conj) eedl))
	    banned blocked-nodes))
	  (disj
	   (let ((eedl2 (remove (cadr disj) eedl)))
	     (do ((nl (caddr disj) (cdr nl)))
		 ((or open-path (null nl))
		  (not open-path))
	       (ext-exp-dag-complete-conjs (cons (car nl) eedl2) banned blocked-nodes))))
	  (t (setq open-path eedl)))))

(defun ext-exp-dag-accessible-kids (eedl banned &optional blocked-nodes)
  (let ((ret nil)
	(mates nil)
	(eunifs nil))
    (do ((eedl2 eedl (cdr eedl2)))
	((null eedl2) ret)
      (let* ((eed (car eedl2))
	     (k (ext-exp-dag-kind eed))
	     (pos (ext-exp-dag-positive eed))
	     (wff (ext-exp-dag-shallow eed)))
	(cond ((or (and (member k '(EQN DEC)) (not pos)
			(wffeq-ab (cdar wff) (cdr wff)))
		   (and (eq k 'TRUE) (not pos))
		   (and (eq k 'FALSE) pos))
	       (push (list 'DONE eed) ret))
	      ((eq k 'EXP)
	       (dolist (arc (ext-exp-dag-arcs eed))
		 (let ((expkid (ext-exp-arc-node arc))
		       (exptrm (ext-exp-arc-exp-term arc)))
		   (unless (or (member expkid blocked-nodes)
			       (intersection banned (free-vars-of exptrm)))
		     (push (list 'EXP eed arc) ret)))))
	      ((eq k 'SEL)
	       (dolist (arc (ext-exp-dag-arcs eed))
		 (let ((selkid (ext-exp-arc-node arc)))
		   (unless (member selkid blocked-nodes)
		     (push (list 'SEL eed arc) ret)))))
	      ((or (member k '(REW NEG))
		   (and (eq k 'CON) pos)
		   (and (member k '(DIS IMP EQN)) (not pos)))
	       (push (list 'CONJ eed (mapcar #'(lambda (x) (ext-exp-arc-node x))
					     (ext-exp-dag-arcs eed)))
		     ret))
	      ((or (and (eq k 'CON) (not pos))
		   (and (member k '(DIS IMP)) pos)
		   (eq k 'DEC))
	       (push (list 'DISJ eed (mapcar #'(lambda (x) (ext-exp-arc-node x))
					     (ext-exp-dag-arcs eed)))
		     ret))
	      ((member k '(EQN EQNGOAL))
	       (dolist (arc (ext-exp-dag-arcs eed))
		 (let* ((n (ext-exp-arc-node arc))
			(a (assoc n eunifs)))
		   (if (and a (not (member n blocked-nodes)))
		       (if (ext-exp-dag-positive eed)
			   (push (list 'EUNIF (cdr a) eed n (ext-exp-arc-kind arc)) ret)
			 (push (list 'EUNIF eed (cdr a) n (ext-exp-arc-kind arc)) ret))
		     (push (cons n eed) eunifs)))))
	      ((eq k 'ATOM)
	       (dolist (arc (ext-exp-dag-arcs eed))
		 (let* ((n (ext-exp-arc-node arc))
			(a (assoc n mates)))
		   (if (and a (not (member n blocked-nodes)))
		       (if (ext-exp-dag-positive eed)
			   (push (list 'MATE (cdr a) eed n) ret)
			 (push (list 'MATE eed (cdr a) n) ret))
		     (push (cons n eed) mates)))))
	      (t nil))))))

; prettify (compare to code in mating-merge-eq.lisp)
(defun ext-exp-dag-prettify (node)
  (let ((fixed-frees nil) ; list of frees
	(frees-to-rename nil) ; list of frees
	(fixed-bounds nil) ; list of bounds
	(bounds-to-rename nil) ; list of bounds
	(alpha nil)  ; renaming for bounds
	(theta nil) ; renaming for frees
	(used-frees nil)) ; cod of theta
    (declare (special fixed-frees frees-to-rename fixed-bounds bounds-to-rename used-frees theta alpha))
    (ext-exp-dag-prettify-process-vars node)
    (when merge-debug
      (msgf "fixed frees: " fixed-frees)
      (msgf "frees to rename: " frees-to-rename)
      (msgf "fixed bounds: " fixed-bounds)
      (msgf "bounds to rename: " bounds-to-rename))
    (dolist (z bounds-to-rename)
      (setf (get z 'not-alpha-image) nil)) ; as we go along, we collect var names to which a bd cannot be sent
    (dolist (y fixed-frees) ; send frees in original wff to themselves (guaranteed to be legal)
      (prettify-free-rename y y))
    (dolist (x fixed-bounds) ; send bounds in original wff to themselves (guaranteed to be legal)
      (prettify-bound-rename x x))
    (dolist (y frees-to-rename) ; on the first pass, find sel var y's which correspond to a bound var in the original wff,
					; and send these to the bv if we can
      (let ((x (get y 'sel-var-bound)))
	(when (and (member x fixed-bounds)
		   (prettify-free-legal-p y x))
	  (prettify-free-rename y x))))
    (dolist (y frees-to-rename) ; on the second pass, find sel var y's which correspond to a bound var x which may be renamed.
					; send x and y to the same (pretty) bv, if we can
      (let ((x (get y 'sel-var-bound)))
	(when x
	  (prettify-identify-free-bound y x))))
    (dolist (z bounds-to-rename) ; on third pass, check to see if any bounds have a preferred var with which to be identified,
					; and try to send them to the same pretty var, if we can
      (dolist (x (get z 'bound-try-to-equate-to-bound))
	(prettify-identify-bound-bound x z))
      (dolist (y (get z 'bound-try-to-equate-to-free))
	(prettify-identify-free-bound y z)))
    (dolist (y frees-to-rename) ; finally, choose names for the rest of the frees
      (when (not (assoc y theta))
	(let ((yp (get-best-alt-free-name y)))
	  (prettify-free-rename y yp))))
    (dolist (z bounds-to-rename) ; and choose names for the rest of the frees
      (when (not (assoc z alpha))
	(let ((zp (get-best-alt-bound-name z)))
	  (prettify-bound-rename z zp))))
    (when merge-debug
      (msgf "renaming for sel vars and free evars:" t theta)
      (msgf "renaming for bound vars:" t alpha))
    (ext-exp-dag-rename-all-vars theta alpha node)))

(defun ext-exp-dag-prettify-process-vars (node)
  (prettify-process-top-shallow (ext-exp-dag-shallow node))
  (let ((nodes-done nil)
	(nodes-delayed nil))
    (declare (special nodes-done nodes-delayed))
    (ext-exp-dag-prettify-process-vars-rec node)))

(defun ext-exp-dag-prettify-process-vars-rec (node)
  (declare (special fixed-frees frees-to-rename nodes-done nodes-delayed))
  (unless (member node nodes-done)
    (push node nodes-done)
    (prettify-process-wff (ext-exp-dag-shallow node))
    (case (ext-exp-dag-kind node)
      (exp
       (dolist (arc (ext-exp-dag-arcs node))
	 (prettify-process-wff (ext-exp-arc-exp-term arc))
	 (ext-exp-dag-prettify-process-vars-rec (ext-exp-arc-node arc))))
      (sel
       (dolist (arc (ext-exp-dag-arcs node))
	 (let ((a (ext-exp-arc-sel-var arc)))
	   (setf (get a 'sel-var-bound) (caar (ext-exp-dag-shallow node)))
	   (unless (or (member a fixed-frees) (member a frees-to-rename))
	     (setf (get a 'free-must-avoid) nil)
	     (push a frees-to-rename)))
	 (ext-exp-dag-prettify-process-vars-rec (ext-exp-arc-node arc))))
      ((atom eqngoal)
       (dolist (arc (ext-exp-dag-arcs node))
	 (let ((node2 (ext-exp-arc-node arc)))
	   (if (member node2 nodes-delayed)
	       (ext-exp-dag-prettify-process-vars-rec node2)
	     (push node2 nodes-delayed)))))
      (eqn
       (if (ext-exp-dag-positive node)
	   (dolist (arc (ext-exp-dag-arcs node))
	     (let ((node2 (ext-exp-arc-node arc)))
	       (if (member node2 nodes-delayed)
		   (ext-exp-dag-prettify-process-vars-rec node2)
		 (push node2 nodes-delayed))))
	 (dolist (arc (ext-exp-dag-arcs node))
	   (ext-exp-dag-prettify-process-vars-rec (ext-exp-arc-node arc)))))
      (t
       (dolist (arc (ext-exp-dag-arcs node))
	 (ext-exp-dag-prettify-process-vars-rec (ext-exp-arc-node arc)))))))

(defun ext-exp-dag-rename-all-vars (theta alpha node)
  (let ((nodes-done nil))
    (declare (special nodes-done))
    (ext-exp-dag-rename-all-vars-1 theta alpha node)))

(defun ext-exp-dag-rename-all-vars-1 (theta alpha node)
  (declare (special nodes-done))
  (let ((a (assoc node nodes-done)))
    (if a
	(cdr a)
      (let ((node2 (copy-eed node)))
	(setf (ext-exp-dag-shallow node2)
	      (rename-all-vars-in-wff theta alpha (ext-exp-dag-shallow node)))
	(setf (ext-exp-dag-arcs node2)
	      (mapcar #'(lambda (arc)
			  (ext-exp-dag-rename-all-vars-2 theta alpha arc))
		      (ext-exp-dag-arcs node)))
	(push (cons node node2) nodes-done)
	node2))))

(defun ext-exp-dag-rename-all-vars-2 (theta alpha arc)
  (let ((arc2 (copy-ext-exp-arc arc)))
    (when (ext-exp-arc-exp-term arc)
      (setf (ext-exp-arc-exp-term arc2)
	    (rename-all-vars-in-wff theta alpha (ext-exp-arc-exp-term arc))))
    (when (ext-exp-arc-sel-var arc)
      (setf (ext-exp-arc-sel-var arc2)
	    (rename-all-vars-in-wff theta alpha (ext-exp-arc-sel-var arc))))
    (setf (ext-exp-arc-node arc2)
	  (ext-exp-dag-rename-all-vars-1 theta alpha (ext-exp-arc-node arc)))
    arc2))

(defun ext-exp-dag-rulep (eedl)
  (let ((acc (ext-exp-dag-accessible-kids eedl nil nil)))
    (if (find-if #'(lambda (x)
		     (or (and (eq (car x) 'DONE)
			      (member (ext-exp-dag-kind (cadr x)) '(TRUE FALSE)))
			 (and (eq (car x) 'MATE)
			      (wffeq (ext-exp-dag-shallow (cadr x))
				     (ext-exp-dag-shallow (caddr x))))))
		 acc)
	t
      (let ((conj (find-if #'(lambda (x)
			       (and (eq (car x) 'CONJ)
				    (or (member (ext-exp-dag-kind (cadr x)) '(NEG CON DIS IMP))
					(and (eq (ext-exp-dag-kind (cadr x)) 'REW)
					     (member (ext-exp-dag-rew-just (cadr x)) '(EQUIV-IMPLICS EQUIV-DISJS))))))
			   acc)))
	(if conj
	    (ext-exp-dag-rulep (append (caddr conj) (remove (cadr conj) eedl)))
	  (let ((disj (find-if #'(lambda (x)
				   (and (eq (car x) 'DISJ)
					(member (ext-exp-dag-kind (cadr x)) '(CON DIS IMP))))
			       acc)))
	    (if disj
		(let ((eedl2 (remove (cadr disj) eedl))
		      (ret t))
		  (do ((nl (caddr disj) (cdr nl)))
		      ((or (not ret) (null nl))
		       ret)
		    (setq ret (ext-exp-dag-rulep (cons (car nl) eedl2)))))
	      nil)))))))

(defun ext-exp-pf-to-ext-seq (eepf)
  (ext-exp-pf-to-ext-seq-l (list eepf) (ext-exp-dag-sel-vars eepf)))

; for debugging - could delete this wrapper and rename ext-exp-pf-to-ext-seq-l-0 to ext-exp-pf-to-ext-seq-l
(defun ext-exp-pf-to-ext-seq-l (eepfl banned)
  (let* ((z (ext-exp-pf-to-ext-seq-l-0 eepfl banned))
	 (wffs1 (mapcar #'(lambda (x)
			    (if (ext-exp-dag-positive x)
				(cons 'NOT (ext-exp-dag-shallow x))
			      (ext-exp-dag-shallow x)))
			eepfl))
	 (wffs2 (ext-seq-wffs z)))
    (when ext-exp-dag-verbose
      (unless (wffeq-ab-multiset wffs1 wffs2)
	(setf (get 'ext-exp-dag-debug 'es) z)
	(setf (get 'ext-exp-dag-debug 'eepfl) eepfl)
	(setf (get 'ext-exp-dag-debug 'banned) banned)
	(throwfail "Multisets translating from ext-exp-dag proof to ext-seq proof do not match")))
    z))

(defun ext-exp-pf-to-ext-seq-l-0 (eepfl banned)
  (let ((nxt (ext-exp-pf-to-ext-seq-l-1 eepfl banned)))
    (if nxt
	(let* ((node (car nxt))
	       (arc (cadr nxt)) ; non-NIL if node is an expansion, selection, (dec) eqn, (mated) atom or (eunif) eqn node
	       (posnode (caddr nxt)) ; non-NIL if node is a (mated) atom or (eunif) eqn node
	       (posarc (cadddr nxt)) ; non-NIL if node is a (mated) atom or (eunif) eqn node
	       (others (remove node eepfl))
	       (k (ext-exp-dag-kind node))
	       (pos (ext-exp-dag-positive node)))
	  (case k
	    (TRUE (make-ext-seq-true
		   (mapcar #'(lambda (x)
			       (if (ext-exp-dag-positive x)
				   (cons 'NOT (ext-exp-dag-shallow x))
				 (ext-exp-dag-shallow x)))
			   others)))
	    (FALSE (make-ext-seq-false
		    (mapcar #'(lambda (x)
				(if (ext-exp-dag-positive x)
				    (cons 'NOT (ext-exp-dag-shallow x))
				  (ext-exp-dag-shallow x)))
			    others)))
	    (NEG
	     (if pos
		 (let ((es (ext-exp-pf-to-ext-seq-l
			    (cons (ext-exp-arc-node (car (ext-exp-dag-arcs node)))
				  others) banned)))
		   (make-ext-seq-neg (cdr (ext-exp-dag-shallow node)) es))
	       (ext-exp-pf-to-ext-seq-l
		(cons (ext-exp-arc-node (car (ext-exp-dag-arcs node)))
		      others) banned)))
	    (DIS
	     (if pos
		 (let ((es1 (ext-exp-pf-to-ext-seq-l
			     (cons (ext-exp-arc-node (car (ext-exp-dag-arcs node)))
				   others) banned))
		       (es2 (ext-exp-pf-to-ext-seq-l
			     (cons (ext-exp-arc-node (cadr (ext-exp-dag-arcs node)))
				   others) banned))
		       (sh (ext-exp-dag-shallow node)))
		   (make-ext-seq-dis-neg (cdar sh) (cdr sh) es1 es2))
	       (let ((es (ext-exp-pf-to-ext-seq-l
			  (cons (ext-exp-arc-node (car (ext-exp-dag-arcs node)))
				(cons (ext-exp-arc-node (cadr (ext-exp-dag-arcs node)))
				      others)) banned))
		     (sh (ext-exp-dag-shallow node)))
		 (make-ext-seq-dis-pos (cdar sh) (cdr sh) es))))
	    (CON
	     (if pos
		 (let ((es (ext-exp-pf-to-ext-seq-l
			    (cons (ext-exp-arc-node (car (ext-exp-dag-arcs node)))
				  (cons (ext-exp-arc-node (cadr (ext-exp-dag-arcs node)))
					others)) banned))
		       (sh (ext-exp-dag-shallow node)))
		   (make-ext-seq-con-neg (cdar sh) (cdr sh) es))
	       (let ((es1 (ext-exp-pf-to-ext-seq-l
			   (cons (ext-exp-arc-node (car (ext-exp-dag-arcs node)))
				 others) banned))
		     (es2 (ext-exp-pf-to-ext-seq-l
			   (cons (ext-exp-arc-node (cadr (ext-exp-dag-arcs node)))
				 others) banned))
		     (sh (ext-exp-dag-shallow node)))
		 (make-ext-seq-con-pos (cdar sh) (cdr sh) es1 es2))))
	    (IMP
	     (if pos
		 (let ((es1 (ext-exp-pf-to-ext-seq-l
			     (cons (ext-exp-arc-node (car (ext-exp-dag-arcs node)))
				   others) banned))
		       (es2 (ext-exp-pf-to-ext-seq-l
			     (cons (ext-exp-arc-node (cadr (ext-exp-dag-arcs node)))
				   others) banned))
		       (sh (ext-exp-dag-shallow node)))
		   (make-ext-seq-imp-neg (cdar sh) (cdr sh) es1 es2))
	       (let ((es (ext-exp-pf-to-ext-seq-l
			  (cons (ext-exp-arc-node (car (ext-exp-dag-arcs node)))
				(cons (ext-exp-arc-node (cadr (ext-exp-dag-arcs node)))
				      others))
			  banned))
		     (sh (ext-exp-dag-shallow node)))
		 (make-ext-seq-imp-pos (cdar sh) (cdr sh) es))))
	    (EXP
	     (let* ((sh (ext-exp-dag-shallow node))
		    (sh2 (if pos (cons 'NOT sh) sh))
		    (node2 (ext-exp-arc-node arc))
		    (trm (ext-exp-arc-exp-term arc))
		    (node3 (when (cdr (ext-exp-dag-arcs node))
			     (make-eea-exp sh pos (remove arc (ext-exp-dag-arcs node)))))
		    (es1 (ext-exp-pf-to-ext-seq-l
			  (if node3
			      (cons node2 (cons node3 others))
			    (cons node2 others))
			  banned))
		    (es2 (if pos
			     (make-ext-seq-forall-neg sh trm es1)
			   (make-ext-seq-exists-pos sh trm es1))))
	       (if node3
		   (make-ext-seq-contr sh2 es2)
		 es2)))
	    (SEL
	     (let* ((sh (ext-exp-dag-shallow node))
		    (sh2 (if pos (cons 'NOT sh) sh))
		    (node2 (ext-exp-arc-node arc))
		    (sv (ext-exp-arc-sel-var arc))
		    (node3 (when (cdr (ext-exp-dag-arcs node))
			     (make-eea-sel sh pos (remove arc (ext-exp-dag-arcs node)))))
		    (es1 (ext-exp-pf-to-ext-seq-l
			  (if node3
			      (cons node2 (cons node3 others))
			    (cons node2 others))
			  (remove sv banned)))
		    (es2 (if pos
			     (make-ext-seq-exists-neg sh sv es1)
			   (make-ext-seq-forall-pos sh sv es1))))
	       (if node3
		   (make-ext-seq-contr sh2 es2)
		 es2)))
	    (DEC
	     (let ((sh (ext-exp-dag-shallow node)))
	       (if (wffeq-ab (cdar sh) (cdr sh))
		   (make-ext-seq-refl-ab sh
					 (mapcar #'(lambda (x)
						     (if (ext-exp-dag-positive x)
							 (cons 'NOT (ext-exp-dag-shallow x))
						       (ext-exp-dag-shallow x)))
						 others))
		 (let ((arcs (ext-exp-dag-arcs node))
		       (esl nil))
		   (dotimes (i (length arcs))
		     (let ((decarc (find-if #'(lambda (x) (equal i (ext-exp-arc-dec-index x))) arcs)))
		       (unless decarc (throwfail "Missing Dec " i " in " node))
		       (push (ext-exp-pf-to-ext-seq-l (cons (ext-exp-arc-node decarc) others) banned) esl)))
		   (apply #'make-ext-seq-dec
			  (cons sh
				(cons (mapcar #'(lambda (x)
						  (if (ext-exp-dag-positive x)
						      (cons 'NOT (ext-exp-dag-shallow x))
						    (ext-exp-dag-shallow x)))
					      others)
				      (reverse esl))))))))
	    (ATOM
	     (let* ((others2 (remove posnode others))
		    (sh (ext-exp-dag-shallow node))
		    (possh (ext-exp-dag-shallow posnode))
		    (arcs1 (ext-exp-dag-arcs node))
		    (arcs2 (ext-exp-dag-arcs posnode))
		    (decnode (ext-exp-arc-node arc))
		    (decarcs (ext-exp-dag-arcs decnode))
		    (esl nil)
		    (es2 nil)
		    (node-copy (when (cdr arcs1)
				 (let ((eed-c (copy-eed node)))
				   (setf (ext-exp-dag-arcs eed-c)
					 (remove arc arcs1))
				   eed-c)))
		    (node2-copy (when (cdr arcs2)
				 (let ((eed-c (copy-eed posnode)))
				   (setf (ext-exp-dag-arcs eed-c)
					 (remove posarc arcs2))
				   eed-c))))
	       (when node-copy (push node-copy others2))
	       (when node2-copy (push node2-copy others2))
	       (dotimes (i (length decarcs))
		 (let ((decarc (find-if #'(lambda (x) (equal i (ext-exp-arc-dec-index x))) decarcs)))
		   (unless decarc (throwfail "Missing Dec " i " in " node))
		   (push (ext-exp-pf-to-ext-seq-l (cons (ext-exp-arc-node decarc) others2) banned) esl)))
	       (setq es2 (apply #'make-ext-seq-init-eq
				(cons possh
				      (cons sh
					    (cons (mapcar #'(lambda (x)
							      (if (ext-exp-dag-positive x)
								  (cons 'NOT (ext-exp-dag-shallow x))
								(ext-exp-dag-shallow x)))
							  others2)
						  (reverse esl))))))
	       (when node-copy (setq es2 (make-ext-seq-contr sh es2)))
	       (when node2-copy (setq es2 (make-ext-seq-contr (cons 'NOT possh) es2)))
	       es2))
	    ((EQN EQNGOAL)
	     (if (or pos (eq k 'EQNGOAL))
		 (if arc
		     (let* ((others2 (remove posnode others))
			    (sh (ext-exp-dag-shallow node))
			    (possh (ext-exp-dag-shallow posnode))
			    (arcs1 (ext-exp-dag-arcs node))
			    (arcs2 (ext-exp-dag-arcs posnode))
			    (conjnode (ext-exp-arc-node arc))
			    (eqnnode1 (ext-exp-arc-node (car (ext-exp-dag-arcs conjnode))))
			    (eqnnode2 (ext-exp-arc-node (cadr (ext-exp-dag-arcs conjnode))))
			    (node-copy (when (cdr arcs1)
					 (let ((eed-c (copy-eed node)))
					   (setf (ext-exp-dag-arcs eed-c)
						 (remove arc arcs1))
					   eed-c)))
			    (node2-copy (when (cdr arcs2)
					  (let ((eed-c (copy-eed posnode)))
					    (setf (ext-exp-dag-arcs eed-c)
						  (remove posarc arcs2))
					    eed-c))))
		       (when node-copy (push node-copy others2))
		       (when node2-copy (push node2-copy others2))
		       (let* ((es1 (ext-exp-pf-to-ext-seq-l (cons eqnnode1 others2) banned))
			      (es2 (ext-exp-pf-to-ext-seq-l (cons eqnnode2 others2) banned))
			      (es3 (if (eq (ext-exp-arc-kind arc) 'EUNIF1)
				       (make-ext-seq-eunif1 possh sh es1 es2)
				     (make-ext-seq-eunif2 possh sh es1 es2))))
			 (when node-copy
			   (setq es3 (make-ext-seq-contr sh es3)))
			 (when node2-copy
			   (setq es3 (make-ext-seq-contr (cons 'NOT possh) es3)))
			 es3))
		   (make-ext-seq-refl-ab (ext-exp-dag-shallow node)
					 (mapcar #'(lambda (x)
						     (if (ext-exp-dag-positive x)
							 (cons 'NOT (ext-exp-dag-shallow x))
						       (ext-exp-dag-shallow x)))
						 others)))
	       (let* ((nodes (mapcar #'(lambda (x) (ext-exp-arc-node x))
				     (ext-exp-dag-arcs node)))
		      (sh (ext-exp-dag-shallow node))
		      (es2 (ext-exp-pf-to-ext-seq-l (append nodes others) banned)))
		 (dotimes (i (length (cdr nodes)))
		   (setq es2 (make-ext-seq-contr sh es2)))
		 es2)))
	    (REW
	     (let* ((sh (ext-exp-dag-shallow node))
		    (arc (car (ext-exp-dag-arcs node)))
		    (kid (ext-exp-arc-node arc))
		    (kidsh (ext-exp-dag-shallow kid))
		    (ak (ext-exp-dag-rew-just node)))
	       (case ak
		 (EQUIVWFFS
		  (make-ext-seq-rew sh kidsh 'EQUIVWFFS (not (ext-exp-dag-positive node))
				    (ext-exp-pf-to-ext-seq-l (cons kid others) banned)))
		 (LAMBDA
		  (make-ext-seq-lambda (if pos (cons 'NOT sh) sh)
				       (ext-exp-pf-to-ext-seq-l (cons kid others) banned)))
		 ((EQUIV-IMPLICS EQUIV-DISJS)
		  (let* ((val (ext-exp-dag-remove-equiv node))
			 (a1 (caar val))
			 (b1 (cdar val))
			 (a2 (cadr val))
			 (b2 (cddr val)))
		    (if pos
			(let ((es1 (ext-exp-pf-to-ext-seq-l (cons a1 (cons b1 others)) banned))
			      (es2 (ext-exp-pf-to-ext-seq-l (cons a2 (cons b2 others)) banned)))
			  (make-ext-seq-equiv-neg (cdar sh) (cdr sh) es1 es2))
		      (let ((es1 (ext-exp-pf-to-ext-seq-l (cons a1 (cons b2 others)) banned))
			    (es2 (ext-exp-pf-to-ext-seq-l (cons a2 (cons b1 others)) banned)))
			(make-ext-seq-equiv-pos (cdar sh) (cdr sh) es1 es2)))))
		 (EXT=
		  (let* ((lft (cdar sh))
			 (rght (cdr sh))
			 (tp (unabbreviated-type lft)))
		    (if (consp tp)
			(let ((es1 (ext-exp-pf-to-ext-seq-l (cons kid others) banned))
			      (x (fresh-var-1 (cdr tp))))
			  (ext-seq-forall-to-funceq
			   es1 sh (acons x 'FORALL
					 (acons (inherit-abbrev '= (cons (cons 'O (car tp)) (car tp)) (list (car tp)))
						(cons lft x) (cons rght x)))
			   (not pos)))
		      (if (eq tp 'O)
			  (let* ((val (ext-exp-dag-remove-equiv kid)) ; fix this
				 (a1 (caar val))
				 (b1 (cdar val))
				 (a2 (cadr val))
				 (b2 (cddr val)))
			    (if pos
				(let ((es1 (ext-exp-pf-to-ext-seq-l (cons a1 (cons b1 others)) banned))
				      (es2 (ext-exp-pf-to-ext-seq-l (cons a2 (cons b2 others)) banned)))
				  (make-ext-seq-eqo sh es2 es1))
			      (let ((es1 (ext-exp-pf-to-ext-seq-l (cons a1 (cons b2 others)) banned))
				    (es2 (ext-exp-pf-to-ext-seq-l (cons a2 (cons b1 others)) banned)))
				(make-ext-seq-exto sh es1 es2))))
			(throwfail "Bad type " tp " for extensionality rewrite node: " node))))))))
	    (t (throwfail "Translation problem " node))))
      (throwfail "Translation from EDag to Seq Proof Reached an Impass " t eepfl))))

; should replace this by using ext-exp-pf-accessible-kids
(defun ext-exp-pf-to-ext-seq-l-1 (eepfl banned)
  (let ((done nil)
	(conj nil)
	(expsel nil)
	(expselarc nil)
	(disj nil)
	(mates nil)
	(eunifs nil)
	(mate1 nil)
	(mate2 nil)
	(matearc1 nil)
	(matearc2 nil)
	(eunif1 nil)
	(eunif2 nil)
	(eunifarc1 nil)
	(eunifarc2 nil))
    (do ((eedl2 eepfl (cdr eedl2)))
	((or done (null eedl2))
	 (if done
	     (list done)
	   (if mate1
	       (if (ext-exp-dag-positive mate1)
		   (list mate2 matearc2 mate1 matearc1)
		 (list mate1 matearc1 mate2 matearc2))
	     (if eunif1
		 (if (ext-exp-dag-positive eunif1)
		     (list eunif2 eunifarc2 eunif1 eunifarc1)
		   (list eunif1 eunifarc1 eunif2 eunifarc2))
	       (if expsel
		   (list expsel expselarc)
		 (if conj
		     (list conj)
		   (if disj
		       (list disj)
		     nil)))))))
      (let* ((eed (car eedl2))
	     (k (ext-exp-dag-kind eed))
	     (pos (ext-exp-dag-positive eed))
	     (wff (ext-exp-dag-shallow eed)))
	(cond ((or (and (member k '(EQN DEC)) (not pos)
			(wffeq-ab (cdar wff) (cdr wff)))
		   (and (eq k 'TRUE) (not pos))
		   (and (eq k 'FALSE) pos))
	       (setq done eed))
	      ((eq k 'SEL)
	       (when (ext-exp-dag-arcs eed)
		 (setq expsel eed expselarc (car (ext-exp-dag-arcs eed)))))
	      ((eq k 'EXP)
	       (dolist (arc (ext-exp-dag-arcs eed))
		 (let ((exp (ext-exp-arc-exp-term arc)))
		   (unless (intersection (free-vars-of exp) banned)
		     (setq expsel eed expselarc arc)))))
	      ((or (eq k 'NEG)
		   (eq k 'REW)
		   (and (eq k 'CON) pos)
		   (and (member k '(DIS IMP EQN)) (not pos)))
	       (setq conj eed))
	      ((or (and (member k '(CON DEC)) (not pos))
		   (and (member k '(DIS IMP)) pos))
	       (setq disj eed))
	      ((member k '(EQN EQNGOAL))
	       (dolist (arc (ext-exp-dag-arcs eed))
		 (let* ((n (ext-exp-arc-node arc))
			(a (assoc n eunifs)))
		   (if a
		       (setq eunif1 eed eunif2 (cadr a) eunifarc1 arc eunifarc2 (caddr a))
		     (push (list n eed arc) eunifs)))))
	      ((eq k 'ATOM)
	       (dolist (arc (ext-exp-dag-arcs eed))
		 (let* ((n (ext-exp-arc-node arc))
			(a (assoc n mates)))
		   (if a
		       (setq mate1 eed mate2 (cadr a) matearc1 arc matearc2 (caddr a))
		     (push (list n eed arc) mates)))))
	      (t nil))))))


(defun eed-essential-p (e)
  (if (or (and (member (ext-exp-dag-kind e) '(ATOM EQNGOAL))
	       (ext-exp-dag-arcs e))
	  (and (eq (ext-exp-dag-kind e) 'EQN)
	       (ext-exp-dag-arcs e)
	       (ext-exp-dag-positive e)))
      t
    (let ((res nil))
      (do ((arcs (ext-exp-dag-arcs e) (cdr arcs)))
	  ((or res (not arcs)) res)
	(setq res (eed-essential-p (ext-exp-arc-node (car arcs))))))))

; just for sanity checking
(defun ext-exp-dag-check-acyclicity-l (eedl)
  (if eedl
      (if (ext-exp-dag-positive (car eedl))
	  (ext-exp-dag-check-acyclicity-l-1 (make-eed-neg (car eedl)) (cdr eedl))
	(ext-exp-dag-check-acyclicity-l-1 (car eedl) (cdr eedl)))
    t))

(defun ext-exp-dag-check-acyclicity-l-1 (eed eedl)
  (if eedl
      (let ((eed2 (if (ext-exp-dag-positive (car eedl))
		      (make-eed-neg (car eedl))
		    (car eedl))))
	(ext-exp-dag-check-acyclicity-l-1 (make-eed-dis eed eed2) (cdr eedl)))
    (ext-exp-dag-sel-acyclicity-p eed)))

(defun ext-exp-dag-check-structure-l (eedl &optional (rewrite-defns-eager NIL))
  (let ((nodes-delayed nil)
	(nodes-checked nil))
    (declare (special nodes-delayed nodes-checked))
    (ext-exp-dag-check-structure-l-1 eedl rewrite-defns-eager)))

(defun ext-exp-dag-check-structure-l-1 (eedl &optional (rewrite-defns-eager NIL))
  (if eedl
      (and (ext-exp-dag-check-structure-1 (car eedl)
					  (ext-exp-dag-positive (car eedl))
					  rewrite-defns-eager)
	   (ext-exp-dag-check-structure-l-1 (cdr eedl)
					    rewrite-defns-eager))
    t))

(defun ext-exp-dag-check-structure-1 (eed pos1 &optional (rewrite-defns-eager NIL))
  (ext-exp-dag-check-structure-2 eed pos1 (ext-exp-dag-shallow eed) rewrite-defns-eager))

(defun ext-exp-dag-check-structure-2 (eed pos1 sh1 &optional (rewrite-defns-eager NIL))
  (declare (special nodes-delayed nodes-checked))
  (if (member eed nodes-checked)
      t
    (let ((sh (ext-exp-dag-shallow eed))
	  (pos (ext-exp-dag-positive eed))
	  (k (ext-exp-dag-kind eed))
	  (arcs (ext-exp-dag-arcs eed)))
      (when (or (not (equal pos1 pos))
		(not (wffeq-ab sh1 sh)))
	(setf (get 'ext-exp-dag-debug 'eed) eed)
	(throwfail "structure error in " eed))
      (push eed nodes-checked)
      (case k
	(TRUE (and (wffeq-ab sh 'TRUTH) (not arcs)))
	(FALSE (and (wffeq-ab sh 'FALSEHOOD) (not arcs)))
	(NEG (and (not-p sh) (= (length arcs) 1)
		  (ext-exp-dag-check-structure-2
		   (ext-exp-arc-node (car arcs)) (not pos) (cdr sh)
		   rewrite-defns-eager)))
	(DIS (and (or-p sh) (= (length arcs) 2)
		  (ext-exp-dag-check-structure-2
		   (ext-exp-arc-node (car arcs)) pos (cdar sh) rewrite-defns-eager)
		  (ext-exp-dag-check-structure-2
		   (ext-exp-arc-node (cadr arcs)) pos (cdr sh) rewrite-defns-eager)))
	(CON (and (and-p sh) (= (length arcs) 2)
		  (ext-exp-dag-check-structure-2
		   (ext-exp-arc-node (car arcs)) pos (cdar sh) rewrite-defns-eager)
		  (ext-exp-dag-check-structure-2
		   (ext-exp-arc-node (cadr arcs)) pos (cdr sh) rewrite-defns-eager)))
	(IMP (and (implies-p sh) (= (length arcs) 2)
		  (ext-exp-dag-check-structure-2
		   (ext-exp-arc-node (car arcs)) (not pos) (cdar sh) rewrite-defns-eager)
		  (ext-exp-dag-check-structure-2
		   (ext-exp-arc-node (cadr arcs)) pos (cdr sh) rewrite-defns-eager)))
	(EXP (and (ae-bd-wff-p sh)
		  (ext-exp-arc-check-structure-exp-1 pos (bindvar sh) (cdr sh) arcs rewrite-defns-eager)))
	(SEL (and (ae-bd-wff-p sh)
		  (ext-exp-arc-check-structure-sel-1 pos (bindvar sh) (cdr sh) arcs rewrite-defns-eager)))
	(REW (and (= (length arcs) 1)
		  (ext-exp-dag-check-structure-1 (ext-exp-arc-node (car arcs)) pos rewrite-defns-eager)))
	(ATOM
	 (let ((ret t))
	   (dolist (arc arcs ret)
	     (let* ((node (ext-exp-arc-node arc))
		    (na (assoc node nodes-delayed)))
	       (if na
		   (let ((pos2 (cadr na))
			 (sh2 (caddr na)))
		     (setq ret (and ret (not (equal pos pos2))
				    (ext-exp-dag-check-structure-2
				     node nil
				     (if pos
					 (acons (inherit-abbrev '= '((O . O) . O) '(O))
						sh2 sh)
				       (acons (inherit-abbrev '= '((O . O) . O) '(O))
					      sh sh2))
				     rewrite-defns-eager))))
		 (push (list node pos sh) nodes-delayed))))))
	(EQN
	 (and (equals-p sh)
	      (if pos
		  (let ((ret t)
			(lft1 (cdar sh))
			(rght1 (cdr sh)))
		    (dolist (arc arcs ret)
		      (let* ((node (ext-exp-arc-node arc))
			     (na (assoc node nodes-delayed)))
			(if na
			    (let* ((lft2 (cadr na))
				   (rght2 (caddr na))
				   (pos2 (nth 3 na))
				   (tp (unabbreviated-type lft1))
				   (q (inherit-abbrev '= (cons (cons 'O tp) tp) (list tp)))
				   (arckind2 (nth 4 na)))
			      (setq ret (and ret (not pos2) (equal arckind2 (ext-exp-arc-kind arc))
					     (equal tp (unabbreviated-type lft2))
					     (ext-exp-dag-check-structure-2
					      node nil
					      (if (eq arckind2 'EUNIF1)
						  (acons 'AND
							 (acons q lft1 lft2)
							 (acons q rght1 rght2))
						(acons 'AND
						       (acons q lft1 rght2)
						       (acons q rght1 lft2)))
					      rewrite-defns-eager))))
			  (push (list node lft1 rght1 t (ext-exp-arc-kind arc)) nodes-delayed)))))
		(and (<= (length arcs) 2)
		     (or (not (car arcs)) (ext-exp-dag-check-structure-2 (ext-exp-arc-node (car arcs)) pos sh rewrite-defns-eager))
		     (or (not (cadr arcs)) (ext-exp-dag-check-structure-2 (ext-exp-arc-node (cadr arcs)) pos sh rewrite-defns-eager))))))
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
		  (let* ((arc (find-if #'(lambda (x) (equal i (ext-exp-arc-dec-index x))) arcs))
			 (arg1 (nth i args1))
			 (arg2 (nth i args2))
			 (tp1 (unabbreviated-type arg1))
			 (tp2 (unabbreviated-type arg2)))
		    (setq ret (and ret (equal tp1 tp2)
				   (ext-exp-dag-check-structure-2
				    (ext-exp-arc-node arc) nil
				    (acons (inherit-abbrev '= (cons (cons 'O tp1) tp1) (list tp1))
					   arg1 arg2) rewrite-defns-eager))))))))
	(EQNGOAL
	 (and (equals-p sh) (not pos)
	      (let ((ret t)
		    (lft2 (cdar sh))
		    (rght2 (cdr sh)))
		(dolist (arc arcs ret)
		  (let* ((node (ext-exp-arc-node arc))
			 (na (assoc node nodes-delayed)))
		    (if na
			(let* ((lft1 (cadr na))
			       (rght1 (caddr na))
			       (pos1 (nth 3 na))
			       (tp (unabbreviated-type lft1))
			       (q (inherit-abbrev '= (cons (cons 'O tp) tp) (list tp)))
			       (arckind1 (nth 4 na)))
			  (setq ret (and ret pos1 (equal arckind1 (ext-exp-arc-kind arc))
					 (equal tp (unabbreviated-type lft2))
					 (ext-exp-dag-check-structure-2
					  node nil
					  (if (eq arckind1 'EUNIF1)
					      (acons 'AND
						     (acons q lft1 lft2)
						     (acons q rght1 rght2))
					    (acons 'AND
						   (acons q lft1 rght2)
						   (acons q rght1 lft2)))
					  rewrite-defns-eager))))
		      (push (list node lft2 rght2 nil (ext-exp-arc-kind arc)) nodes-delayed)))))))
	(LEAF t)
	(t (throwfail "Unknown node " eed))))))

(defun ext-exp-arc-check-structure-exp-1 (pos1 x sh arcs &optional (rewrite-defns-eager NIL))
  (if arcs
      (let* ((exptrm (ext-exp-arc-exp-term (car arcs)))
	     (exptrm0 (if rewrite-defns-eager
			  (etanorm (lambda-norm ; expand all abbrevs in exp term here
				    (instantiate-all-rec exptrm '(EQUIV))))
			exptrm))
	     (sh2 (substitute-l-term-var exptrm0 x sh)))
	(and (ext-exp-dag-check-structure-2 (ext-exp-arc-node (car arcs)) pos1 sh2 rewrite-defns-eager)
	     (ext-exp-arc-check-structure-exp-1 pos1 x sh (cdr arcs) rewrite-defns-eager)))
    t))

(defun ext-exp-arc-check-structure-sel-1 (pos1 x sh arcs &optional (rewrite-defns-eager NIL))
  (if arcs
      (let* ((sv (ext-exp-arc-sel-var (car arcs)))
	     (sh2 (substitute-l-term-var sv x sh)))
	(and (ext-exp-dag-check-structure-2 (ext-exp-arc-node (car arcs)) pos1 sh2 rewrite-defns-eager)
	     (ext-exp-arc-check-structure-sel-1 pos1 x sh (cdr arcs) rewrite-defns-eager)))
    t))

; 9/19/2004 - ceb - if rewrite-defns-eager is T, then expand any abbrevs in exp terms
; - we do this while translating from ext seq proofs (to make merging easier),
;  then explicitly put in EQUIVWFF rewrites (using ext-exp-dag-contract-defns)
;  after the whole ext exp dag is constructed.
(defun ext-exp-dag-check-pf-p (eedl &key (rewrite-defns-eager NIL))
  (and (ext-exp-dag-check-structure-l eedl rewrite-defns-eager)
       (ext-exp-dag-complete-l eedl)
       (ext-exp-dag-check-acyclicity-l eedl)))

(defun ext-exp-dag-self-contained-p (eed)
  (ext-exp-dag-self-contained-l-p (list eed)))

(defun ext-exp-dag-self-contained-l-p (eedl)
  (let ((nodes-delayed nil)
	(nodes-checked nil))
    (declare (special nodes-delayed nodes-checked))
    (ext-exp-dag-self-contained-l-p-1 eedl)
    (not nodes-delayed)))

(defun ext-exp-dag-self-contained-l-p-1 (eedl)
  (declare (special nodes-delayed nodes-checked))
  (when eedl
    (let ((eed (car eedl)))
      (if (member eed nodes-checked)
	  t
	(let ((kids (mapcar #'(lambda (x)
				(ext-exp-arc-node x))
			    (ext-exp-dag-arcs eed)))
	      (k (ext-exp-dag-kind eed)))
	  (push eed nodes-checked)
	  (if (or (eq k 'ATOM) (eq k 'EQNGOAL)
		  (and (eq k 'EQN) (ext-exp-dag-positive eed)))
	      (let ((kids2 nil))
		(dolist (kid kids)
		  (if (member kid nodes-delayed)
		      (progn
			(setq nodes-delayed (remove kid nodes-delayed))
			(push kid kids2))
		    (push kid nodes-delayed)))
		(ext-exp-dag-self-contained-l-p-1 (append kids2 (cdr eedl))))
	    (ext-exp-dag-self-contained-l-p-1 (append kids (cdr eedl)))))))))

(defun ext-exp-dag-self-contained-nodes (eed)
  (ext-exp-dag-self-contained-nodes-l (list eed)))

(defun ext-exp-dag-self-contained-nodes-l (eedl)
  (let ((nodes-yes nil)
	(nodes-no nil))
    (declare (special nodes-yes nodes-no))
    (ext-exp-dag-self-contained-nodes-l-1 eedl)
    nodes-yes))

(defun ext-exp-dag-self-contained-nodes-l-1 (eedl)
  (declare (special nodes-yes nodes-no))
  (when eedl
    (let* ((eed (car eedl))
	   (done (or (member eed nodes-yes) (member eed nodes-no)))
	   (kids (if done
		     nil
		   (mapcar #'(lambda (x)
			       (ext-exp-arc-node x))
			   (ext-exp-dag-arcs eed)))))
      (unless done
	(if (ext-exp-dag-self-contained-p eed)
	    (push eed nodes-yes)
	  (push eed nodes-no)))
      (ext-exp-dag-self-contained-nodes-l-1
       (append kids (cdr eedl))))))

(defun ext-exp-dag-kids (node &optional k)
  (mapcar #'(lambda (arc) (ext-exp-arc-node arc))
	  (if k
	      (remove-if-not #'(lambda (arc)
				 (eq (ext-exp-arc-kind arc) k))
			     (ext-exp-dag-arcs node))
	    (ext-exp-dag-arcs node))))

(defun eed-mated-p (node1 node2)
  (intersection (ext-exp-dag-kids node1 'MATE)
		(ext-exp-dag-kids node2 'MATE)))

(defun eed-eunifk-p (node1 node2 euk)
  (intersection (ext-exp-dag-kids node1 euk)
		(ext-exp-dag-kids node2 euk)))

(defun eed-eunif1-p (node1 node2)
  (eed-eunifk-p node1 node2 'EUNIF1))

(defun eed-eunif2-p (node1 node2)
  (eed-eunifk-p node1 node2 'EUNIF2))

(defun ext-exp-dag-below-lambdaeqw (x)
  (if (and (eq (ext-exp-dag-kind x) 'REW)
	   (member (ext-exp-dag-rew-just x) '(LAMBDA EQUIVWFFS)))
      (ext-exp-dag-below-lambdaeqw (car (ext-exp-dag-kids x)))
    x))
  
; Need:
; checking acyclicity
; computing jforms (through a given set of nodes)?
; checking no vp's through a given set
