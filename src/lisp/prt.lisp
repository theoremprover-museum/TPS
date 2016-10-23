;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :core)
(part-of WFF-PRINT)

;;;
;;; File PRT
;;;
;;; defines the main recursive functions used to print and pretty-print
;;; wffs.

(deffile prt
  (part-of wff-print)
  (extension clisp)
  (mhelp "Contains functions for printing and pretty-printing wffs."))

(context wff-printing)


;;; This symbol defined as a hack, to get around TOPS20 problem
;;; with compiling periods.  Use period rather than \. or |.|.
(defvar period (intern "."))

(defun printwffhere (wff)
  (prtwff wff
	  (ppwfflag nil)
	  (displaywff nil)
	  (leftmargin (curpos))
	  (rightmargin (linelength nil))
	  (atomvalflag nil)))

;;;Make sure that curpos has the right value before calling this function

;;;Before printing a encoded lambda term, we have to transform it into an untyped
;;;one. This is used for untyped lambda calculus.
(defun decode-typed (wff)
  (cond ((not (consp wff)) wff)
        ((eq 'cl-user::LAM<I<II>> (car wff)) (decode-typed (cdr wff)))
        ((eq 'cl-user::APP<III> (car wff)) (decode-typed (cdr wff)))
        (t (cons (decode-typed (car wff)) (decode-typed (cdr wff))))))

;;;This function used to return nil. I'm changing it so that it returns
;;; HATOMALIST instead. The reason is that I want to look at HATOMALIST to
;;; decide whether to print types or not in the justifications in
;;;function JUSTLENGTH (FILE PRTOTL)

(defflag elim-defns
  (flagtype boolean)
  (default nil)
  (subjects printing library)
  (mhelp "When printing a wff, first instantiate all of the definitions
and lambda-normalize. This instantiation will ignore REWRITE-DEFNS, but 
will use the current setting of REWRITE-EQUALITIES.
It's best to leave this at NIL (i.e. off), since output with it set to T can 
be confusing."))

(defun eliminate-defns (wff)
  (do ((g wff (lnorm (instantiate-all (instantiate-equalities g) '(equiv))))
       (oldg nil g))
      ((wffeq-ab g oldg) g)))

(defun pwff (wff)
  (declare (special atomposlist atomvallist markatom untyped-lambda-calculus))
  (let ((wff (if untyped-lambda-calculus (decode-typed wff) 
	       (if elim-defns (eliminate-defns wff) wff)))
        (*print-case* :upcase)
	(ppvirtflag nil)
	(ppwfflength 0)
	(pc " ")
	(hatomalist nil))
    (declare (special ppvirtflag ppwfflength pc hatomalist))
    (when atomvalflag
      (setq atomposlist nil)
      (setq atomvallist nil)
      (setq markatom nil))
    (if displaywff (pcall display-prefix) (pcall text-prefix))
    (if ppwfflag
	(let (pplist lastprefindt)
	  (declare (special lastprefindt))
	  (setq ppvirtflag t)
	  (setq pplist (if scope (printwffscope wff nil 1)
			   (printwffplain wff t 1))
		ppvirtflag nil)
	  (setq lastprefindt
		(if (singleaplicn (caar pplist)) leftmargin curpos))
	  (printpplist pplist leftmargin)
	  (if atomvalflag (atomvalterpri)))
	(if scope (printwffscope wff nil 1) (printwffplain wff t 1)))
    (if displaywff (pcall display-postfix)
	(pcall text-postfix))
    hatomalist))


(defun printwffscope (wff brackets depth)
  (declare (special hatomalist))
  (let ((*print-case* :upcase))
    (cond ((prt-symbol-p wff)
	   (printwffplain wff brackets depth))
	  (ppvirtflag
	   (let ((beforelength ppwfflength)
		 (space-p (pcall print-space-p "[" pc))
		 (dot-flag (and brackets use-dot (null allscopeflag)))
		 aplicnlist ppwffdepth)
	     (setq ppwfflist nil)
	     (setq pc (if dot-flag "." "["))
	     (incf ppwfflength (1+ (if space-p 1 0)))
	     (setq aplicnlist (printwff wff t depth))
	     (incf ppwfflength (if dot-flag 0 1))
	     (setq ppwffdepth (if (genchar (car aplicnlist))
				  (- ppwfflength beforelength)
				(+ (aplicnlistdepth aplicnlist)
				   (if dot-flag 0 1))))
	     (acons aplicnlist
		    (cons ppwffdepth
			  (if dot-flag (if space-p 'space-dot 'dot)
			    (if space-p 'space-brackets 'brackets)))
		    (- ppwfflength beforelength))))
	  (t (if (and brackets use-dot (null allscopeflag))
		 (pp-symbol-space ".")
	       (pp-symbol-space "["))
	     (printwff wff t depth)
	     (if (not (and brackets use-dot (null allscopeflag)))
		 (pp-symbol-space "]"))))))

(defun printwffplain (wff brackets depth)
  (declare (special hatomalist))
  (cond ((and (prt-aplicn-p wff)
	      (or (prt-infix-op (gar wff)) (prt-infix-op (gdr wff))))
	 (printwffscope wff nil depth))
	(ppvirtflag (let ((beforelength ppwfflength)
			  aplicnlist ppwffdepth)
		      (setq ppwfflist nil)
		      (setq aplicnlist (printwff wff brackets depth))
		      (setq ppwffdepth
			    (if (genchar (car aplicnlist))
				(- ppwfflength beforelength)
				(aplicnlistdepth aplicnlist)))
		      (acons aplicnlist (list ppwffdepth)
			     (- ppwfflength beforelength))))
	(t (printwff wff brackets depth))))

(defwffrec printwff (argnames wff brackets depth))

(defun printwff (wff brackets depth)
  (declare (special hatomalist printtypes))
  (let ((*print-case* :upcase))
    (cond ((label-q wff)
	   (apply-label wff (printwff wff brackets depth)))
	  ((lsymbol-q wff)
	   (if printtypes
	       (let* ((nr (getnameroot wff))
		      (typ (cdr (assoc nr hatomalist))))
		 (if (or (and (not printtypes-all) typ (eq typ wff))
			 (not (type wff))) ; cebrown 5/28/03
		     (pp-symbol-space nr)
		   (progn (push (cons nr wff) hatomalist)
			  (pp-lsymbol-space wff))))
	     (pp-symbol-space (getnameroot wff))))
	  ((= depth printdepth) (pp-symbol-space '&))
	  ((and (prt-aplicn-p (car wff)) (prt-infix-op (gar (car wff))))
	   (prog (plainflag slevelflag prior inf left-arg left conn right)
	     (setq inf (gar (car wff)))
	     (setq prior (prt-infix-op inf))
	     (setq left-arg (gdr (car wff)))
	     (setq plainflag (need-no-brackets prior left-arg))
	     (setq slevelflag
	       (and plainflag (prt-aplicn-p left-arg)
		    (prt-aplicn-p (gar left-arg))
		    (prt-infix-op (gar (gar left-arg)))
		    (= prior (prt-infix-op (gar (gar left-arg))))
		    (prt-associative-p (gar (gar left-arg)))))
	     (setq left
	       (funcall
		(cond (slevelflag #'printwff)
		      (plainflag #'printwffplain)
		      (t #'printwffscope))
		left-arg nil (if slevelflag depth (1+ depth))))
	     (setq conn (printwffplain inf nil depth))
	     (setq plainflag (need-no-brackets-on-right prior (cdr wff)))
	     (setq right
	       (funcall (if plainflag #'printwffplain #'printwffscope)
			(cdr wff) brackets (1+ depth)))
	     (return (if ppvirtflag
			 (nconc (if slevelflag left
				   (list (cons '((nil 0) . 0) left)))
				(list (cons conn right)))
		       nil))))
	  ((boundwff-q wff)
	   (if ppvirtflag
	       (let ((beforelength ppwfflength)
		     binderpatom binderpgl)
		 (pp-symbol-space (cdar wff))
		 (setq binderpatom (printwff (caar wff) brackets depth))
		 (setq binderpgl
		   (acons binderpatom (list (- ppwfflength beforelength))
			  (- ppwfflength beforelength)))
		 (list (cons binderpgl
			     (funcall
			      (if (or (and (prt-aplicn-p (cdr wff))
					   (boundwff-q (cdr wff))
					   (null allscopeflag))
				      (and (get (cdar wff) 'prefix)
					   (need-no-brackets
					    (get (cdar wff) 'prefix)
					    (cdr wff))))
				  #'printwffplain #'printwffscope)
			      (cdr wff) brackets (1+ depth)))))
	     (progn
	       (pp-symbol-space (cdar wff))
	       (printwff (caar wff) brackets depth)
	       (funcall
		(if (or (and (prt-aplicn-p (cdr wff)) (boundwff-q (cdr wff))
			     (null allscopeflag))
			(and (get (cdar wff) 'prefix)
			     (need-no-brackets (get (cdar wff) 'prefix)
					       (cdr wff))))
		    #'printwffplain #'printwffscope)
		(cdr wff) brackets (1+ depth)))))
	  (t (list
	      (cons (funcall
		     (if (or
			  (and
			   (prt-aplicn-p (car wff))
			   (or (boundwff-q (car wff))
			       (and (prt-aplicn-p (gar (car wff)))
				    (prt-infix-op (gar (gar (car wff)))))))
			  allscopeflag)
			 #'printwffscope #'printwffplain)
		     (car wff) nil (1+ depth))
		    (funcall
		     (if (and (null allscopeflag)
			      (prt-symbol-p (car wff))
			      (prt-prefix-op (car wff))
			      (need-no-brackets (prt-prefix-op (car wff))
						(cdr wff) t))
			 #'printwffplain #'printwffscope)
		     (cdr wff) brackets (1+ depth))))))))

(defun need-no-brackets (prior scope &optional (prefix nil))
  (and (null allscopeflag)
       (or (lsymbol-q scope)
	   (and (prt-aplicn-p scope)
		(prt-aplicn-p (gar scope))
		(or (not (prt-infix-op (gar (gar scope))))
		    (if prefix 
                        (< prior (prt-infix-op (gar (gar scope))))
                        (not (< (prt-infix-op (gar (gar scope))) prior)))))
	   (and (prt-aplicn-p scope)
		(prt-symbol-p (gar scope))
		(or (not (prt-prefix-op (gar scope)))
		    (not (< (prt-prefix-op (gar scope)) prior)))))))


(defun need-no-brackets-on-right (prior scope)
  (and (null allscopeflag)
       (or (prt-symbol-p scope)
	   (and (prt-aplicn-p scope)
		(prt-aplicn-p (gar scope))
		(or (not (prt-infix-op (gar (gar scope))))
		    (> (infix-op-p (gar (gar scope))) prior)))
	   (and (prt-aplicn-p scope)
		(prt-symbol-p (gar scope))
		(or (not (prt-prefix-op (gar scope)))
		    (> (prt-prefix-op (gar scope)) prior))))))

(defun pp-lsymbol-space (lsymbol)
  (let ((*print-case* :upcase))
    (if (and (not ppvirtflag) (pcall terpri-heuristics lsymbol))
	(setq pc " "))
    (if (pcall print-space-p lsymbol pc) (pcall print-symbol " "))
    (if (member lsymbol '("." " "  "[" "]") :test #'string=)
	(pcall print-symbol lsymbol)
      (pp-lsymbol lsymbol))
    (setq pc lsymbol)
    (if ppvirtflag ppwfflist lsymbol)))

(defun pp-symbol-space (symbol)
  (let ((*print-case* :upcase))
    (if (and (not ppvirtflag) (pcall terpri-heuristics symbol))
	(setq pc " "))
    (if (pcall print-space-p symbol pc) (pcall print-symbol " "))
    (pcall print-symbol symbol)
    (setq pc symbol)
    (if ppvirtflag ppwfflist symbol)))


(defun pp-space-p (symbol pc no-space-p)
  (cond ((member pc '(" " "." "[" "~") :test #'string=) nil)
	((string-equal pc "NOT") nil)
	((string= symbol "[") T)
	((member symbol '("." " " "]") :test #'string=) nil)
	(t (if first-order-print-mode
	       (or (and (symbolp pc) (get pc 'fo-single-symbol))
		   (get symbol 'fo-single-symbol))
	       (not no-space-p)))))

(defun pp-lsymbol (lsymbol)
  (let ((*print-case* :upcase)
	(name-base (getnameroot lsymbol)))
    (pcall print-symbol name-base)
    (if (and printtypes 
	     (or (and printtypes-all (get name-base 'typelist))
		 (and (not (get lsymbol 'printnotype))
		      (not (get name-base 'printnotype)))))
	(pcall print-typesym (type lsymbol)))
    lsymbol))

(defun pp-typesym (tp)
  (let ((*print-case* :upcase))
    (cond ((null tp)
	   ;;An illegal type symbol was somehow created, eg during
	   ;;parsing - don't print anything.
	   )
	  ((atom tp) (pcall print-type-char tp))
	  (t (pp-typesym (car tp))
	     (cond ((consp (cdr tp))
		    (pcall print-symbol "(")
		    (pp-typesym (cdr tp))
		    (pcall print-symbol ")"))
		   (t (pp-typesym (cdr tp))))))))

(defun flatsym (sym)
  (let ((ppwfflist nil)
	(ppvirtflag t)
	(ppwfflength 0))
    (declare (special ppvirtflag ppwfflength ppwfflist))
    (if (eq sym 'or) (pprinc " "))
    (pcall print-symbol sym)
    (if (or (eq sym 'or)
	    (and (memq sym '(forall exists)) ;;otherwise we get forallx
		 (memq style '(concept generic))))
	(pprinc " "))
    (cons ppwfflist ppwfflength)))

(defun flatstring (string)
  (cons (list string) (length string)))

(defun flatwff (wff)
  (let ((rightmargin 9999)
	(printdepth 0)
	(ppvirtflag t)
	(ppwfflength 0)
	(pc " ")
	(ppwfflist nil)
	(curpos 0)
	(hatomalist nil))
    (declare (special printdepth ppvirtflag ppwfflength ppwfflist curpos
		      pc hatomalist))
    (printwffplain wff nil 1)))


