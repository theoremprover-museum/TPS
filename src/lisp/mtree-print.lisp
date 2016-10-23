;;; -*- Mode:LISP; Package:auto -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;
(in-package :AUTO)


(deffile mtree-print
  (part-of mst)
  (extension lsp)
  (mhelp "Defines printing functions for matingstree."))

(context mtree-print)

(defmtreeop pmtr-flat
  (mtree-alias print-matingstree)
  (mtree-args 1)
  (mtree-default (default))
  (mhelp "Print out the given matingstree in a flat format. 
If no matingstree is given, the current matingstree is printed out."))

(defwffop print-matingstree 
  (argnames name)
  (argtypes symbol-or-integer)
  (mhelp "Print out the given matingstree. If no matingstree is given,
the current-matingstree is printed out.")
  (resulttype matingstree))

(defun print-matingstree (name)
  (print-matingstree-main 
     (if (eq name 'default)
         current-matingstree
         (mst-goto name current-matingstree)))
  (if (neq name 'default) (mst-goto (matingstree-name current-matingstree) current-matingstree)))

(defun print-matingstree-main (matingstree)
  (msg T (matingstree-name matingstree) 3)
  (let ((hxsons (matingstree-sons matingstree)))
     (cond ((null hxsons)
           (mst-show-mating-real matingstree))
           (T (mapc #'(lambda (son) (msg (matingstree-name son) 2))
                 hxsons)
              (mapc #'print-matingstree-main hxsons)))))

(context mtree-ops)

(defflag default-ob
  (flagtype obdefault)
  (default 'd-smallest)
  (subjects mtree-top etrees transmit)
  (mhelp "If DEEPEST, the default next obligation is found by depth-first
search of the obtree, if HIGHEST it is found by breadth-first-search, 
if D-SMALLEST then the deepest of the set of smallest obligations (i.e.
the set of all obligations with the fewest possible literals) is 
chosen, if H-SMALLEST then the highest of this set is chosen."))

(definfo deepest
  (mhelp "A setting for DEFAULT-OB.
The default next obligation in mtree is found by depth-first search
of the obligation tree."))

(definfo highest
  (mhelp "A setting for DEFAULT-OB.
The default next obligation in mtree is found by breadth-first search
of the obligation tree.

A setting for MT-DEFAULT-OB-MATE
When applying ADD-CONN to an mtree, choose the default obligation by
choosing the obligation which lies highest (i.e. nearest to the
root, but not the root itself)."))

(definfo d-smallest
  (mhelp "A setting for DEFAULT-OB.
The default next obligation in mtree is the deepest element of the 
set of smallest obligations (i.e. given the set of all obligations with
the fewest possible literals, the first element of this set to be found
by depth-first search)."))

(definfo d-highest
  (mhelp "A setting for DEFAULT-OB.
The default next obligation in mtree is the highest element of the 
set of smallest obligations (i.e. given the set of all obligations with
the fewest possible literals, the first element of this set to be found
by breadth-first search)."))

(context mtree-print)

;;;----------------------------------------------------------
(defmtreeop pmtr*
  (mtree-alias tr-print-matingstree-ob)
  (mtree-args 1)
  (mtree-default (default))
  (mhelp "Print out the given matingstree as a tree, showing the obligations at each 
node. If no matingstree is given, the current-matingstree is printed out. 

Numbers in round brackets are open obligations. If the brackets end in \"..\",
there are too many open obligations to fit under the mstree label. 

Leaves underlined with ^'s are closed matingstrees. 
Matingstrees enclosed in curly brackets are marked as dead.
Branches with *'s denote nodes that are being omitted for lack of space. The 
cure for this is to either start printing from a node lower in the tree, or 
make the screen wider. See also PMTR."))

(defwffop tr-print-matingstree-ob
  (argnames name)
  (argtypes symbol-or-integer)
  (mhelp "Print out the given matingstree as a tree, showing the obligations at each 
node. If no matingstree is given, the current-matingstree is printed out. 

Numbers in round brackets are open obligations. If the brackets end in \"..\",
there are too many open obligations to fit under the mstree label. 

Leaves underlined with ^'s are closed matingstrees. 
Matingstrees enclosed in curly brackets are marked as dead.
Branches with *'s denote nodes that are being omitted for lack of space. The 
cure for this is to either start printing from a node lower in the tree, or 
make the screen wider.")
  (resulttype matingstree))

(defun tr-print-matingstree-ob (name)
  (tr-print-matingstree-main leftmargin rightmargin  
   (if (eq name 'default)
       current-matingstree
     (mst-goto name current-matingstree)) t)
  (if (neq name 'default) (mst-goto (matingstree-name current-matingstree) current-matingstree)))

(defmtreeop pmtr
  (mtree-alias tr-print-matingstree)
  (mtree-args 1)
  (mtree-default (default))
  (mhelp "Print out the given matingstree as a tree, showing the obligations at each 
node. If no matingstree is given, the current-matingstree is printed out. 

Matingstrees enclosed in curly brackets are marked as dead.
Branches with *'s denote nodes that are being omitted for lack of space. The 
cure for this is to either start printing from a node lower in the tree, or 
make the screen wider. See also PMTR*."))

(defwffop tr-print-matingstree 
  (argnames name)
  (argtypes symbol-or-integer)
  (mhelp "Print out the given matingstree as a tree, showing the obligations at each 
node. If no matingstree is given, the current-matingstree is printed out. 

Matingstrees enclosed in curly brackets are marked as dead.
Branches with *'s denote nodes that are being omitted for lack of space. The 
cure for this is to either start printing from a node lower in the tree, or 
make the screen wider.")
  (resulttype matingstree))

(defun tr-print-matingstree (name)
  (tr-print-matingstree-main leftmargin rightmargin  
   (if (eq name 'default)
       current-matingstree
     (mst-goto name current-matingstree)))
  (if (neq name 'default) (mst-goto (matingstree-name current-matingstree) current-matingstree)))

(defun tr-print-matingstree-main (l r matingstree &optional (print-obs nil))
  (let ((center (round (/ (+ l r) 2)))
	(label-width (+ (length (princ-to-string (matingstree-name matingstree))) 2))
	(obwidth (length (princ-to-string (get-open-obs (matingstree-obligation matingstree)))))
	(old-tabstops nil))
    (if print-obs (msgf "Numbers in round brackets are open obligations. If the brackets 
end in \"..\", there are too many open obligations to fit 
under the mstree label. Leaves underlined with ^'s are 
closed matingstrees. Matingstrees enclosed in curly brackets are
marked as dead. Branches with *'s denote nodes that are being 
omitted for lack of space." t)
      (msgf "Branches with *'s denote nodes that are being omitted for lack of space.
Matingstrees in curly brackets are marked as dead." t))
    (msg T T (t (- center (round (/ label-width 2)))) (pldb matingstree)
	 (matingstree-name matingstree) (prdb matingstree) t)
    (if print-obs 
	(if (get-open-obs (matingstree-obligation matingstree))
	    (msg (t (- center (round (/ obwidth 2)))) (get-open-obs (matingstree-obligation matingstree)) t)
	  (msg (t (- center (round (/ label-width 2)))) (make-string label-width :initial-element #\^))))
    (setq old-tabstops (list (cons center "|")))
    (do ((plist (matingstree-sons matingstree) (rec-sons plist))
	 (old-plist nil plist))
	((equal plist old-plist))
	(if old-plist (setq old-tabstops (rec-print l r old-plist label-width print-obs old-tabstops))))))

(defun rec-print (l r plist lw print-obs &optional (old-tabstops nil))
  (let ((tabstops (closeness-filter (mapcar #'(lambda (x) (cons (round (car x)) (cdr x)))
					    (sort (calc-tabstops l r plist) 
						  #'(lambda (x y) (< (car x) (car y))))) lw )))
;    (dolist (tb old-tabstops) (if (string= (char (cdr tb) 0) "*") (msg (t (car tb)) "*") (msg (t (car tb)) "|")))
    (dolist (tb old-tabstops) (msg (t (car tb)) "|"))
    (msg t)
    (do* ((i 0 (1+ i))
	  (oldshape " " (or newshape " "))
	  (newshape (cdr (assoc i tabstops)) (cdr (assoc i tabstops))))
	 ((> i (caar (last tabstops))))
	 (if newshape (progn 
			(if (> (length newshape) 1) (setq newshape (char newshape 1)))
			(msg newshape) 
			(if (or (string= newshape "/") (string= newshape "+"))
			    (setq newshape "-")
			  (setq newshape " ")))
	   (progn (if (string= oldshape "-") (setq newshape "-")) (msg oldshape))))
    (msg t)
;    (dolist (tb tabstops) (if (string= (char (cdr tb) 0) "*") (msg (t (car tb)) "*") (msg (t (car tb)) "|")))
    (dolist (tb tabstops) (msg (t (car tb)) "|"))
    (msg t)
    (mapcar #'(lambda (x y) (p-mst-elt x y lw)) tabstops (remove-if #'null (flatten-mstlist plist)))
    (if print-obs (progn (msg t)
	(mapcar #'(lambda (x y) (p-mst-obs x y lw)) tabstops (remove-if #'null (flatten-mstlist plist)))))
    (msg t)
    tabstops))

(defun closeness-filter (alist lw)
  (if (< (length alist) 2) alist
    (let ((first (car alist))
	  (second (cadr alist)))
      (if (> (1+ lw) (- (car second) (car first)))
	  (cons (if (string= (char (cdr first) 0) "*") first (cons (car first) (concatenate 'string "*" (cdr first))))
		(closeness-filter (cons (cons (car second) (concatenate 'string "*" (cdr second))) (cddr alist)) lw))
	(if (> (1+ (/ 2 lw)) (- core:rightmargin (car second)))
	    (cons first (closeness-filter (cons (cons (car second) (concatenate 'string "*" (cdr second))) (cddr alist)) lw))
	  (cons first (closeness-filter (cdr alist) lw)))))))

(defun p-mst-elt (tb l lw)
  (if (string= (char (cdr tb) 0) "*") (msg (t (car tb)) "*")
  (msg (t (- (car tb) (round (/ lw 2)))) (pldb l) l (prdb l))))

(defun p-mst-obs (tb l lw)
  (let* ((to-print (get-open-obs (matingstree-obligation l)))
	 (st1 (mapcar #'(lambda (x) (string-left-trim "OB" x)) (mapcar #'princ-to-string to-print)))
	 (to-print (let ((x (car st1)))
		     (dolist (s (cdr st1))
			     (setq x (concatenate 'string x " " s)))
		     x))
	 (string (if to-print (concatenate 'string "(" to-print ")")
		   (make-string lw :initial-element #\^)))
	 (string (if (> (length string) lw) (progn (setf (aref string (- lw 2)) #\.)
						   (setf (aref string (1- lw)) #\.)
						   (adjust-array string lw)) string))
	 (obwidth (length string)))
    (if (string= (char (cdr tb) 0) "*") (msg (t (car tb)) "*")
      (if (> obwidth lw) (msg (t (car tb)) "*")
	(msg (t (- (car tb) (round (/ obwidth 2)))) string)))))
	   
(defun rec-sons (plist)
  (if (null plist) nil
    (if (listp (car plist)) (cons (rec-sons (car plist)) (rec-sons (cdr plist)))
      (cons (matingstree-sons (car plist)) (rec-sons (cdr plist))))))
;;;----------------------------------------------------------
(defmtreeop pm-node
  (mtree-alias print-matingstree-node)
  (mtree-args 1)
  (mtree-default (default))
  (mhelp "Print out the given matingstree node in detail. If no 
node is given, the current matingstree is used."))

(defwffop print-matingstree-node 
  (argnames name)
  (argtypes symbol-or-integer)
  (mhelp "Print out the given matingstree node in detail. If no 
node is given, the current matingstree is used.")
  (resulttype matingstree))

(defun print-matingstree-node (name)
  (pm-node-main 
   (if (eq name 'default)
       current-matingstree
     (mst-goto name current-matingstree)))
  (if (neq name 'default) (mst-goto (matingstree-name current-matingstree) current-matingstree)))

(defun pm-node-main (m)
  (msg t "Name : " (matingstree-name m) t 
       "Parent : " (matingstree-parent m) t
       "Children : " (matingstree-sons m) t
       "Literal pair : " (matingstree-literal-pair m) t
       "Merged : " (matingstree-merged m) t
       "Dead : " (matingstree-dead m) t
       "Obligation : " (matingstree-obligation m) t t
       "Mating so far : " (mst-show-mating-real m) t))

;;;----------------------------------------------------------
(defmtreeop pob-lits
  (mtree-alias print-obligation-literal)
  (mtree-args 1)
  (mtree-default (default))
  (mhelp "Print out the unblocked literals in a given obligation tree.
If no argument is given, the current-obligation tree is the default."))

(defwffop print-obligation-literal
  (argnames name)
  (argtypes symbol-or-integer)
  (mhelp "Print out the unblocked literals in a given obligation tree.
If no argument is given, the current-obligation tree is the default."))

(defmtreeop potr-flat
  (mtree-alias print-obligation)
  (mtree-args 1)
  (mtree-default (default))
  (mhelp "Print out the given obligation tree in flat form, 
with the jforms attached to the leaves. If no argument is 
given, the current-obligation tree is printed out."))

(defmtreeop potr*-flat
  (mtree-alias pprint-obligation)
  (mtree-args 1)
  (mtree-default (default))
  (mhelp "Print out the given obligation tree in flat form, 
with the jforms attached to all nodes. If no argument is given,
the whole obligation tree is printed out."))

(defwffop print-obligation
  (argnames name)
  (argtypes symbol-or-integer)
  (mhelp "Print out the given obligation tree with the jforms attached to the
leaves. If no argument is given, the current-obligation tree is printed out."))

(defwffop pprint-obligation
  (argnames name)
  (argtypes symbol-or-integer)
  (mhelp "Print out the given obligation tree with the jforms attached to all nodes. 
If no argument is given,the whole obligation tree is printed out."))

(eval-when (compile eval load)
(defmacro display-vp-diag-brief (gwff)
  `(apply #'vpform (vpformdefaults ,gwff '$ '$ '$ 'll '$ '$)))
)

(defun print-obligation-literal-real (obligation)
 (or (obligation-closed obligation)
  (let* ((hxnext (obligation-next obligation))
         (leaf (null hxnext))
         (literal (eq (obligation-type obligation) 'literal)))
     (setq hxnext (remove-if #'obligation-closed hxnext))
     (if leaf
         (progn (princ (obligation-name obligation))
                (if literal 
		    (progn (msg (t 8) (literal-name (obligation-jform obligation)) 
				"    possible mates: "
			   (query-ob-quiet (princ-to-string (obligation-jform obligation)) (princ-to-string (obligation-name obligation)))
			   t))
		  (display-vp-diag-brief (obligation-jform obligation))))
         (mapc #'print-obligation-literal-real hxnext)))))

(defun print-obligation-literal (name)
   (print-obligation-literal-real
      (if (eq name 'default)
          current-obligation
          (obt-goto name current-obligation))))

(defun print-obligation-main-real (detail obligation)
  (msg T (obligation-name obligation) 3)
  (flet ((ftemp (x) (print-obligation-main-real detail x)))
     (let* ((hxnext (obligation-next obligation))
            (leaf (null hxnext)))
       (if (not detail) (setq hxnext (remove-if #'obligation-closed hxnext)))
       (cond (leaf (obt-show-leaves obligation))
             (T (mapc #'(lambda (son) (msg (obligation-name son) 2)) hxnext)
                (if detail (display-vp-diag (obligation-jform obligation)))
                (mapc #'ftemp hxnext))))))

(defun print-obligation (name)
   (print-obligation-main-real nil
     (if (eq name 'default)
         current-obligation
         (obt-goto name current-obligation))))

(defun pprint-obligation (name)
   (print-obligation-main-real t
     (if (eq name 'default)
         current-obligation
         (obt-goto name current-obligation))))

(defun obt-show-leaves (obligation)
    (let ((closed (obligation-closed obligation)))
         (if closed (msg "Closed: " closed)))
    (display-vp-diag (obligation-jform obligation)))


(defmtreeop ppath
  (mtree-alias print-obligation-path)
  (mtree-args 1)
  (mtree-default (default))
  (mhelp "Print out the path containing the given obligation.
If no obligation is specified, then the first open obligation 
in the current obligation tree is used. See the flag 
DEFAULT-OB-DEEP."))

(defwffop print-obligation-path
  (argnames name)
  (argtypes symbol-or-integer)
  (mhelp "Print out the path containing the given obligation.
If no obligation is specified, then the first open obligation 
in the current obligation tree is used. See the flag 
DEFAULT-OB-DEEP.")
  (resulttype obligation))
  
(defun print-obligation-path (name)
   (print-obligation-path-real 
     (if (eq name 'default)
	 (if (get-open-obs)
	     (obt-goto (get-default-ob) current-obligation)
	   (throwfail t "No open obligations in this obligation tree" t t)) 
         (obt-goto name current-obligation))))

(defun print-obligation-path-real (obligation)
  (do ((hxlast obligation (obligation-last hxlast)))
      ((null hxlast))
      (msg T (obligation-name hxlast))))

(defmtreeop ppath*
  (mtree-alias pprint-obligation-path)
  (mtree-args 1)
  (mtree-default (default))
  (mhelp "Print out the path containing the given obligation,
and show all of the obligations on this path.
If no obligation is specified, then the first open obligation 
in the current obligation tree is used. See the flag 
DEFAULT-OB-DEEP."))

(defwffop pprint-obligation-path
  (argnames name)
  (argtypes symbol-or-integer)
  (mhelp "Print out the path containing the given obligation,
and show all of the obligations on this path.
If no obligation is specified, then the first open obligation 
in the current obligation tree is used. See the flag 
DEFAULT-OB-DEEP.")
  (resulttype obligation))

(defun pprint-obligation-path (name)
   (pprint-obligation-path-real 
     (if (eq name 'default)
	 (if (get-open-obs)
	     (obt-goto (get-default-ob) current-obligation)
	   (throwfail t "No open obligations in this obligation tree" t t))
       (obt-goto name current-obligation))))

(defun pprint-obligation-path-real (obligation)
  (do ((hxlast obligation (obligation-last hxlast)))
      ((null hxlast))
      (progn (msg T (obligation-name hxlast))
	     (display-vp-diag (obligation-jform hxlast)))))

(defmtreeop pob
  (mtree-alias print-obligation-jform)
  (mtree-args 1)
  (mtree-default (default))
  (mhelp "Print out the vpform associated with the given obligation node.
If no obligation is specified, then the first open obligation 
in the current obligation tree is used. See the flag 
DEFAULT-OB-DEEP."))

(defwffop print-obligation-jform
  (argnames name)
  (argtypes symbol-or-integer)
  (mhelp "Print out the vpform associated with the given obligation node.
If no obligation is specified, then the first open obligation 
in the current obligation tree is used. See the flag 
DEFAULT-OB-DEEP.")
  (resulttype obligation))
  
(defun print-obligation-jform (name)
   (print-obligation-jform-unreal 
     (if (eq name 'default)
	 (if (get-open-obs)
	     (obt-goto (get-default-ob) current-obligation)
	   (throwfail t "No open obligations in this obligation tree" t t))
       (obt-goto name current-obligation))))

(defun print-obligation-jform-unreal (ob)
  (msgf (obligation-name ob) t)
  (print-obligation-jform-real ob))

(defun print-obligation-jform-real (obligation)
  (display-vp-diag (obligation-jform obligation)))

;;;--------------------------------------------------------------------------
(defmtreeop pob-node
  (mtree-alias print-obtree-node)
  (mtree-args 1)
  (mtree-default (default))
  (mhelp "Print out the given obligation in detail. If no 
obligation is given, then the first open obligation 
in the current obligation tree is used. See the flag 
DEFAULT-OB-DEEP."))

(defwffop print-obtree-node 
  (argnames name)
  (argtypes symbol-or-integer)
  (mhelp "Print out the given  obligation in detail. If no 
obligation is given, then the first open obligation 
in the current obligation tree is used. See the flag 
DEFAULT-OB-DEEP.")
  (resulttype matingstree))

(defun print-obtree-node (name)
  (pob-node-main 
   (if (eq name 'default)
       (if (get-open-obs)
	   (obt-goto (get-default-ob) current-obligation)
	 (throwfail t "No open obligations in this obligation tree" t t))
     (obt-goto name current-obligation))))

(defun pob-node-main (m)
  (msg t "Name : " (obligation-name m) t 
       "Last : " (obligation-last m) t
       "Next : " (obligation-next m) t
       "Closed : " (obligation-closed m) t
       "Jform : " (obligation-jform m) t
       "Used-univs : " (obligation-used-univs m) t
       "Disjunction : " (obligation-disjunction m) t
       "From Expanding : " (obligation-from-expanding m) t
       "Eligible List : " (obligation-eligible-list m) t
       "Other : " (obligation-other m) t t))

;;;---------------------------------------------------------------------------
(defmtreeop potr
  (mtree-alias tr-pobtree)
  (mtree-args 1)
  (mtree-default (default))
  (mhelp "Print out the given obligation tree as a tree. If no obligation is given,
the tree below the current obligation is printed out. 

Numbers in round brackets are open obligations; those in square brackets are
closed. 
Branches with *'s denote nodes that are being omitted for lack of space.
The cure for this is to either start printing from a node lower in the tree,
or make the screen wider."))

(defwffop tr-pobtree
  (argnames name)
  (argtypes symbol-or-integer)
  (mhelp "Print out the given obligation tree as a tree. If no obligation is given,
the tree below the current obligation is printed out. 

Numbers in round brackets are open obligations; those in square brackets are
closed. 
Branches with *'s denote nodes that are being omitted for lack of space.
The cure for this is to either start printing from a node lower in the tree,
or make the screen wider.")
  (resulttype matingstree))

(defun tr-pobtree (name)
  (tr-pobtree-main leftmargin rightmargin  
   (if (eq name 'default)
       current-obligation
     (obt-goto name current-obligation))))

(defun tr-pobtree-main (l r obligation)
  (let ((center (round (/ (+ l r) 2)))
	(label-width (+ (length (princ-to-string (obligation-name obligation))) 2))
	(mn-width (length (princ-to-string (matingstree-name current-matingstree))))
	(old-tabstops nil))
    (msgf "Numbers in round brackets are open obligations; those in square 
brackets are closed. Branches with *'s denote nodes that are 
being omitted for lack of space." t)
    (msg t t (t (1+ (- center (round (/ mn-width 2))))) (matingstree-name current-matingstree) t (t (- center (round (/ label-width 2)))) (plb obligation) (obligation-name obligation) (prb obligation) t)
    (setq old-tabstops (list (cons center "|")))
    (do ((plist (obligation-next obligation) (rec-next plist))
	 (old-plist nil plist))
	((equal plist old-plist))
	(if old-plist (setq old-tabstops (rec-o-print l r old-plist label-width old-tabstops))))))

(defun rec-o-print (l r plist lw &optional (old-tabstops nil))
  (let ((tabstops (closeness-filter (mapcar #'(lambda (x) (cons (round (car x)) (cdr x)))
					    (sort (calc-tabstops l r plist) 
						  #'(lambda (x y) (< (car x) (car y))))) lw )))
    (dolist (tb old-tabstops) (msg (t (car tb)) "|"))
    (msg t)
    (do* ((i 0 (1+ i))
	  (oldshape " " (or newshape " "))
	  (newshape (cdr (assoc i tabstops)) (cdr (assoc i tabstops))))
	 ((> i (caar (last tabstops))))
	 (if newshape (progn 
			(if (> (length newshape) 1) (setq newshape (char newshape 1)))
			(msg newshape) 
			(if (or (string= newshape "/") (string= newshape "+"))
			    (setq newshape "-")
			  (setq newshape " ")))
	   (progn (if (string= oldshape "-") (setq newshape "-")) (msg oldshape))))
    (msg t)
    (dolist (tb tabstops) (msg (t (car tb)) "|"))
    (msg t)
    (mapcar #'(lambda (x y) (p-obs-elt x y lw)) tabstops (remove-if #'null (flatten-mstlist plist)))
    (msg t)
    tabstops))

(defun p-obs-elt (tb l lw)
  (if (string= (char (cdr tb) 0) "*") (msg (t (car tb)) "*")
  (msg (t (- (car tb) (round (/ lw 2)))) (plb l) l (prb l))))

;Print Left Bracket
(defun plb (obligation)
  (if (obligation-closed obligation) "[" "("))

(defun pldb (mst)
  (if (eq (matingstree-dead mst) t) "{" "["))

;Print Right Bracket
(defun prb (obligation)
  (if (obligation-closed obligation) "]" ")"))

(defun prdb (mst)
  (if (eq (matingstree-dead mst) t) "}" "]"))

(defun rec-next (plist)
  (if (null plist) nil
    (if (listp (car plist)) (cons (rec-next (car plist)) (rec-next (cdr plist)))
      (cons (obligation-next (car plist)) (rec-next (cdr plist))))))
;;;----------------------------------------------------------
(defun get-open-obs (&optional (obligation current-obligation))
  (let ((rl nil))
    (unless (obligation-closed obligation)
	    (setq rl (list (obligation-name obligation))))
    (do ((plist (obligation-next obligation) (rec-next plist))
	 (old-plist nil plist))
	((equal plist old-plist) rl)
	(if old-plist (setq rl (append (rec-find-open old-plist) rl))))))

(defun rec-find-open (plist)
  (let ((returned nil))
    (dolist (ob (remove-if #'null (flatten-mstlist plist)))
	    (unless (obligation-closed ob) (setq returned (cons (obligation-name ob) returned))))
    returned))

(defun get-open-leaf-obs (&optional (obligation current-obligation))
  (remove-if #'(lambda (x) (obligation-next x)) (get-open-obs-2 obligation)))

(defun get-open-obs-2 (&optional (obligation current-obligation))
  (let ((rl nil))
    (unless (obligation-closed obligation)
	    (setq rl (list obligation)))
    (do ((plist (obligation-next obligation) (rec-next plist))
	 (old-plist nil plist))
	((equal plist old-plist) rl)
	(if old-plist (setq rl (append (rec-find-open-2 old-plist) rl))))))

(defun rec-find-open-2 (plist)
  (let ((returned nil))
    (dolist (ob (remove-if #'null (flatten-mstlist plist)))
	    (unless (obligation-closed ob) (setq returned (cons ob returned))))
    returned))


(defun get-default-ob (&optional (obligation current-obligation))
  (if (eq default-ob 'deepest) (obligation-name (car (get-open-leaf-obs obligation)))
    (if (eq default-ob 'highest) (obligation-name (find-highest (get-open-leaf-obs obligation)))
      (if (eq default-ob 'd-smallest) (obligation-name (find-smallest (get-open-leaf-obs obligation) '>))
	(obligation-name (find-smallest (get-open-leaf-obs obligation) '<))))))
;devious difference - get-open-leaf-obs returns the obs; get-open-obs returns their names.

(defun find-smallest (oblist pred)
  (let ((obr (car oblist))
	(obs (ob-size (car oblist)))
	(obh (ob-height (car oblist))))
  (dolist (ob (cdr oblist))
	  (if (< (ob-size ob) obs)
	      (progn (setq obr ob) (setq obs (ob-size (car oblist))) (setq obh (ob-height (car oblist))))
	    (if (and (= (ob-size ob) obs) (apply pred (list (ob-height ob) obh)))
		(progn (setq obr ob) (setq obs (ob-size (car oblist))) (setq obh (ob-height (car oblist)))))))
  obr))

(defun ob-size (ob)
  (length (jform-to-literal-list (obligation-jform ob) nil)))

(defun find-highest (oblist)
  (let ((obr (car oblist))
	(obh (ob-height (car oblist))))
  (dolist (ob (cdr oblist))
	  (when (< (ob-height ob) obh)
		(setq obr ob) (setq obh (ob-height (car oblist)))))
  obr))

(defun ob-height (ob)
  (if (obligation-last ob) (1+ (ob-height (obligation-last ob)))
    0))

;;;----------------------------------------------------------
(defmtreeop live-leaves
  (mtree-alias print-live-leaves)
  (mtree-args 1)
  (mtree-default (default))
  (mhelp "Print out all of the live leaves in the tree below 
the given matingstree node. If no node is given, the root 
node is used."))

(defwffop print-live-leaves
  (argnames name)
  (argtypes symbol-or-integer)
  (mhelp "Print out all of the live leaves in the tree below 
the given matingstree node. If no node is given, the root 
node is used.")
  (resulttype matingstree))

(defun print-live-leaves (name)
  (let* ((ll (get-live-leaves-main 
	      (if (eq name 'default)
		  (mst-goto 'mstroot current-matingstree)
		(mst-goto name current-matingstree))))
	 (llc (remove-if #'(lambda (x) (get-open-obs (matingstree-obligation x))) ll)))
    (msgf (mapcar #'matingstree-name ll) t)
    (if llc (msg "Of which: " llc " are closed.")
      (msg "None of these are closed.")))
  (mst-goto (matingstree-name current-matingstree) current-matingstree))

(defun get-live-leaves-main (matingstree)
  (let ((hxsons (matingstree-sons matingstree)))
    (if (and (null hxsons) (neq (matingstree-dead matingstree) t))
	(list matingstree)
      (unless (null hxsons)
	      (reduce #'append (mapcar #'get-live-leaves-main hxsons))))))

;;;------------------------------------
(defmtreeop conns-added
  (mtree-alias mst-conns-added)
  (mtree-args 1)
  (mtree-default (default))
  (mhelp "Print out all of the connections which have already 
been added to the given matingstree node. If no node
is given, the current node is used."))

(defwffop mst-conns-added
  (argnames name)
  (argtypes symbol-or-integer)
  (mhelp "Print out all of the connections which have already 
been added to the given matingstree node. If no node
is given, the current node is used.")
  (resulttype matingstree))
  
(defun mst-conns-added (name)
  (let ((ll (matingstree-sons
	     (if (eq name 'default)
		 current-matingstree
	       (mst-goto name current-matingstree)))))
    (dolist (l ll) (msgf "Added " (cons (show-other-name (car (matingstree-literal-pair l)))
					(show-other-name (cdr (matingstree-literal-pair l))))
			 " to get node " (matingstree-name l) t))))
