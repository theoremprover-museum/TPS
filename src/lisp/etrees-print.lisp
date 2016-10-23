;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;
(in-package :auto)
(part-of expansion-tree)

(deffile etrees-print
  (part-of expansion-tree)
  (extension lisp)
  (mhelp "Functions for printing etrees and proofs."))

(context wff-printing)

(eval-when (load compile eval)
(defmacro defmateprtop (op &rest props)
  `(defmateop ,op
     ,@props
     (mate-result-> ignore)
     (matewff-argname gwff)))
)

(push '(defmateprtop . mateop) global-definelist)

(defmateprtop p (mate-alias pnode))
(defprtop pnode
  (mhelp "Print the current node"))

(defun pnode (etree)
  (print-etree etree *standard-output* nil))

(defmateprtop pp (mate-alias pwnode))

(defmateprtop ppf (mate-alias pproof))
(defprtop pproof (mhelp "Print the current proof."))

(defun pproof (etree)
  (declare (ignore etree))
  (let ((*print-eproof-verbose* t))
    (print-eproof current-eproof *standard-output* nil)))

(defprtop pwdeep
  (mhelp "Print the deep formula of an expansion tree."))

(defmateprtop pdeep (mate-alias pwdeep))

(defun pwdeep (gwff)
  (prtwff gwff (displaywff t) (print-deep t) (print-nodenames nil)))

(defprtop ppwdeep
  (mhelp "Pretty-print the deep formula of an expansion tree."))

(defmateprtop ppdeep (mate-alias ppwdeep))

(defun ppwdeep (gwff)
  (prtwff gwff (displaywff t) (ppwfflag t) (printdepth 0)
	  (print-deep t) (print-nodenames nil)))

(defprtop pwshallow
  (mhelp "Print the shallow formula of an expansion tree."))

(defmateprtop psh (mate-alias pwshallow))

(defun pwshallow (gwff)
  (prtwff gwff (displaywff t) (print-deep nil) (print-nodenames nil)))

(defprtop pwnode
  (mhelp "Print an expansion tree with node-names."))

(defmateprtop ppnode (mate-alias pwnode))

(defun pwnode (gwff)
  (let ((etree (if (etree-q gwff) gwff
		   (if (symbolp gwff) (find-etree-node-name gwff))))
	(print-nodenames nil))
    (if etree
	(print-etree etree *standard-output* 0))))

(defprtop display-etree
  (mhelp "Etree Display: print an expansion tree into list form,
printing shallow formulas for leaf nodes only. The format used is
NODE [selection and expansion terms] ; CHILDREN or SHALLOW FORMULA"))

(defprtop display-etree-all
  (mhelp "Etree Print: print an expansion tree into list form, 
printing shallow formulas for all nodes. The format used is 
NODE [selection and expansion terms] ; CHILDREN ; SHALLOW FORMULA"))

(defmateprtop etd
  (mate-alias display-etree))

(defmateprtop etp
  (mate-alias display-etree-all))

(defun display-etree (etree)
  (display-etree-main
   (if (etree-q etree) etree
       (gwff-to-etree etree skolem-default t))))

(defun display-etree-main (etree)
  (msg T (show-other-name (etree-name* etree)))
  (cond 
    ((null (etree-components* etree))
     (msg "  ;  " ((get-shallow etree) . gwff)))
    (T (typecase etree
		 ((or skolem selection expansion)
		  (msg " [ ")
		  (dolist (x (sel-exp-terms etree)) (msg (x . gwff) " "))
		  (msg "]  ;  "))
		 (rewrite (msg " [ " (rewrite-justification etree) " ] ; "))
		 (t (msg "  ;  ")))
       (mapc #'(lambda (son) (msg (show-other-name (etree-name* son)) " "))
	     (etree-components* etree))
       (mapc #'display-etree-main (etree-components* etree)))))


(defun display-etree-all (etree)
  (display-etree-all-main
   (if (etree-q etree) etree
       (gwff-to-etree etree skolem-default t))))


(defun display-etree-all-main (etree)
  (msg T (show-other-name (etree-name* etree)))
  (cond 
    ((null (etree-components* etree))
     (msg " ; " ((get-shallow etree) . gwff)))
    (T (typecase etree
		 ((or skolem selection expansion)
		  (msg " [ ")
		  (dolist (x (sel-exp-terms etree)) (msg (x . gwff) " "))
		  (msg "] ; "))
		 (rewrite (msg " [ " (rewrite-justification etree) " ] ; "))
		 (t (msg " ; ")))
       (mapc #'(lambda (son) (msg (show-other-name (etree-name* son)) " "))
	     (etree-components* etree))
       (msg "; ")
       (prtwff (get-shallow etree) (displaywff nil) (print-deep nil) (print-nodenames nil))
       (mapc #'display-etree-all-main (etree-components* etree)))))

(defun print-etree (etree stream level)
  (declare (ignore level))
  (when etree
  (let ((*standard-output* stream))
  (princ (show-other-name (etree-name* etree)))
  (unless print-nodenames
    (if print-deep 
	(progn (terpri) (princ "Deep formula:  ") (printwffhere (get-deep etree)))
      (progn (terpri) (princ "Shallow formula:  ") (printwffhere (get-shallow etree))))
    (if (etree-positive etree)
	(progn (terpri) (princ "Node is positive."))
      (progn (terpri) (princ "Node is negative.")))
    (when (etree-components* etree)
	  (terpri) (princ "Components:  ")
	  (dolist (x (etree-components* etree))
		  (princ (show-other-name (etree-name* x))) (princ "  ")))
    (when (and (etree-components* etree)
	       (memq (type-of etree) '(skolem selection expansion)))
      (terpri) (princ "Terms:  ")
      (dolist (x (sel-exp-terms etree))
	      (printwffhere x) (princ "  ")))
    (when (and (etree-components* etree)
	       (rewrite-p etree))
      (terpri) (princ "Justification:  ") (princ (rewrite-justification etree)))
    (terpri) (princ "Status:  ") (princ (etree-status* etree))))))

(defprtop etree-to-list
  (mhelp "Print an expansion tree into list form."))

(defun etree-to-list (wff)
  (eval
    `(in-mode re-read
	      (msg ,@(print-etree-main wff)))))

(defun print-etree-main (wff)
  (typecase wff
    ((or true leaf false)
     (list " (|" 
	   (prin1-to-string wff) "| "
	   (prin1-to-string (type-of wff)) " "
	   `(',(get-shallow wff) . gwff) " " (etree-status* wff) ") "))
    ((or edisjunction econjunction implication negation)
     (append
       (list " (|" 
             (prin1-to-string wff) "| "
	     (prin1-to-string (type-of wff)) " "
	     `(',(get-shallow wff) . gwff)
	     " " (etree-status* wff)
            " ("       )
       (mapcan #'print-etree-main (etree-components wff))
       (list  ") ") ))
    ((or selection skolem expansion)
     (append
       (list "(|" 
	     (prin1-to-string wff) "| " 
	     (prin1-to-string (type-of wff)) " "
	     `(',(get-shallow wff) . gwff)
	     " " (etree-status* wff)
	    " (" )
       (mapcan #'(lambda (x) `(',x . gwff))
	       (sel-exp-terms wff))
       (list ") (")
       (mapcan #'print-etree-main (etree-components wff))
       (list "))")))
    (rewrite
     (append
       (list "(|" 
	     (prin1-to-string wff) "| "
	     (prin1-to-string (type-of wff)) " "
	     `(',(get-shallow wff) . gwff)
	     " " (etree-status* wff) " ")
       (print-etree-main (car (etree-components wff)))
       (list (prin1-to-string(rewrite-justification wff)) ")" )))
    (otherwise (throwfail wff " is not an etree."))))

(defmateop ptree (mate-alias tr-print-etree))

(defprtop tr-print-etree 
  (mhelp "Print out the etree below the current topnode, showing expansion
variables, skolem terms, selection terms, and rewrite justifications.
Branches with *'s denote nodes that are being omitted for lack of space. The 
cure for this is to either start printing from a node lower in the tree, or 
make the screen wider, or use SHOWNOTYPES. See also PTREE*"))

(defmateop ptree* (mate-alias tr-print-etree*))

(defprtop tr-print-etree* 
  (mhelp "Print out the etree below the current topnode, showing expansion
variables, skolem terms, selection terms, and rewrite justifications. For
all other nodes, show the shallow formula at that node.
Branches with *'s denote nodes that are being omitted for lack of space. The 
cure for this is to either start printing from a node lower in the tree, or 
make the screen wider, or use SHOWNOTYPES. See also PTREE"))

(defmateop ptree-file 
  (mate-alias tr-print-etree-file*)
  (mate-result-> ignore)
  (matewff-argname etree))

(defwffop tr-print-etree-file*
  (argtypes etree filespec posinteger boolean)
  (argnames etree file width fmlas)
  (arghelp "Etree" "File to output" "Width of output, in columns" "Print shallow fmlas?")
  (defaultfns (lambda (a b c d) (list a (if (eq b '$) (namestring (make-pathname% :name "etree" :type "txt")) b)
				    (if (eq c '$) 200 c) (if (eq d '$) t d))))
  (mhelp "As for PTREE or PTREE*, but send the output to a file.
For a width of 200 characters, you can print the results using
some variant of the following:
\"enscript -r -fCourier-Bold6 -dberyl <filename> \""))

(defun tr-print-etree-file* (etree filespec width fmlas)
  (let ((style 'generic)
	(leftmargin 0) (rightmargin (1- width)))
    (reroute-output filespec *default-pathname-defaults* 
		    (tr-print-etree-main 0 (1- width) etree fmlas))
    (msgf "Done. For a width of 200 characters, you can print the results using
some variant of the following:
\"enscript -r -fCourier-Bold6 -dberyl " (namestring filespec) "\"")))
  
(defun tr-print-etree (etree)
  (tr-print-etree-main leftmargin rightmargin etree))

(defun tr-print-etree* (etree)
  (tr-print-etree-main leftmargin rightmargin etree t))

(defun tr-print-etree-main (l r etree &optional (print-wffs nil))
  (let ((center (round (/ (+ l r) 2)))
	(oblabel (typecase etree
		   ((or skolem selection expansion rewrite) (get-new-label etree print-wffs))
		   (t "")))
	(old-tabstops nil))
    (msgf "Branches with *'s denote nodes that are being omitted for lack of space." t)
    (msg T T (t (- center (round (1+ (/ (length (princ-to-string (etree-name etree))) 2)))))
	 (p-left-bracket etree) (etree-name etree) (p-right-bracket etree) t)
    (when (> (length oblabel) 0)
      (msg (t (- center (round (/ (real-label-length oblabel) 2)))) oblabel t))
    (setq old-tabstops (list (cons center "|")))
    (do ((plist (etree-components etree) (rec-etree-sons plist))
	 (old-plist nil plist))
	((equal plist old-plist))
	(if old-plist (setq old-tabstops (rec-etree-print l r old-plist old-tabstops print-wffs))))))

(defun rec-etree-print (l r plist old-tabstops print-wffs)
  (let ((tabstops (mapcar #'(lambda (x) (cons (round (car x)) (cdr x)))
					    (sort (calc-tabstops l r plist) 
						  #'(lambda (x y) (< (car x) (car y)))))))
    (dolist (tb old-tabstops) (msg (t (car tb)) "|"))
    (msg t) (setq core::curpos 0)
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
    (msg t) (setq core::curpos 0)
    (dolist (tb tabstops) (msg (t (car tb)) "|"))
    (msg t) (setq core::curpos 0)
    (mapcar #'(lambda (x y z w) (p-etree-elt x y z w))
	    tabstops (remove-if #'null (flatten-mstlist plist))
	    (append (cdr tabstops) (list (cons rightmargin 'foo)))
	    (cons (cons leftmargin 'foo) tabstops))
    (msg t) (setq core::curpos 0)
    (mapcar #'(lambda (x y z w) (p-etree-labels x y z w print-wffs))
	    tabstops 
	    (remove-if #'null (flatten-mstlist plist))
	    (append (cdr tabstops) (list (cons rightmargin 'foo)))
	    (cons (cons leftmargin 'foo) tabstops))
    (msg t) (setq core::curpos 0)
    tabstops))

(defun rec-etree-sons (plist)
  (if (null plist) nil
    (if (listp (car plist)) (cons (rec-etree-sons (car plist)) (rec-etree-sons (cdr plist)))
      (cons (etree-components (car plist)) (rec-etree-sons (cdr plist))))))

(defun p-etree-labels (tb l nxtb lsttb pw)
  (let* ((string (get-new-label l pw))
	 (obwidth (real-label-length string)))
    (if (string= (char (cdr tb) 0) "*") 
	(progn (spaces (1- (- (car tb) (1+ core::curpos))))
	  (msg (t (car tb)) "*") (setq core::curpos (car tb)))
      (if (>= obwidth (* 2 (min (1- (- (car nxtb) (car tb)))
				(1- (- (car tb) (max core::curpos (car lsttb)))))))
	  (progn (spaces (1- (- (car tb) (1+ core::curpos))))
		 (msg (t (car tb)) "*")  (setq core::curpos (car tb)))
	(if (= obwidth 0) (progn (spaces (1- (- (car tb) (1+ core::curpos))))
				 (msg (t (car tb)) "|")  (setq core::curpos (car tb)))
	  (progn
	    (spaces (1- (- (- (car tb) (round (/ obwidth 2))) (1+ core::curpos))))
	    (msg (t (- (car tb) (round (/ obwidth 2)))) string)
	    (setq core::curpos (1- (+ obwidth (- (car tb) (round (/ obwidth 2))))))))))))

(defun real-label-length (string)
  (let ((count 0))
    (do ((i 0 (1+ i)))
	((= i (length string)) count)
      (if (< (char-int (elt string i)) 32)
	  (setq i (+ i 3))
	(incf count)))))

(defun p-etree-elt (tb l nxtb lsttb)
  (let* ((string (concatenate 'string (p-left-bracket l) (princ-to-string (etree-name l)) (p-right-bracket l)))
	 (obwidth (length string)))
    (if (string= (char (cdr tb) 0) "*") 
	(progn (spaces (1- (- (car tb) (1+ core::curpos))))
	  (msg (t (car tb)) "*") (setq core::curpos (car tb)))
      (if (>= obwidth (* 2 (min (1- (- (car nxtb) (car tb)))
				(1- (- (car tb) (max core::curpos (car lsttb)))))))
	  (progn (spaces (1- (- (car tb) (1+ core::curpos))))
		 (msg (t (car tb)) "*")  (setq core::curpos (car tb)))
	(if (= obwidth 0) (progn (spaces (1- (- (car tb) (1+ core::curpos))))
				 (msg (t (car tb)) "|")  (setq core::curpos (car tb)))
	  (progn
	    (spaces (1- (- (- (car tb) (round (/ obwidth 2))) (1+ core::curpos))))
	    (msg (t (- (car tb) (round (/ obwidth 2)))) string)
	    (setq core::curpos (1- (+ obwidth (- (car tb) (round (/ obwidth 2))))))))))))

(defun p-left-bracket (l) (if (zerop (etree-status* l)) "{" "["))
(defun p-right-bracket (l) (if (zerop (etree-status* l)) "}" "]"))

(defun get-new-label (etree pw)
  (let (string)
    (typecase etree
      ((or skolem selection expansion)
       (progn (setq string (concatenate 'string (p-left-bracket etree) " "))
	      (dolist (x (sel-exp-terms etree)) 
		(setq string (concatenate 'string string (with-output-to-string (*standard-output*) (msg (x . gwff) " ")))))
	      (setq string (concatenate 'string string (p-right-bracket etree)))))
      (rewrite (setq string (concatenate 'string (p-left-bracket etree)
					 (princ-to-string (rewrite-justification etree))
					 (p-right-bracket etree))))
      (t (if pw (setq string (with-output-to-string (*standard-output*) (msg ((get-shallow etree) . gwff))))
	   (setq string ""))))
    string))
