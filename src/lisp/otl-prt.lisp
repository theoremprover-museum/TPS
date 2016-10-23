;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of OTLNL)

;;; File: otl-prt
;;; Package: otlnl
;;;

(deffile otl-prt
  (part-of otlnl)
  (extension clisp)
  (mhelp "Commands for looking at parts of the proof, and wffs in proof."))

(context otl-printing)

(defmexpr depth
  (argtypes integer+)
  (argnames num)
  (arghelp "non-negative integer")
  (defaultfns (lambda (num) (cond ((eq num '$)(list printdepth))
				  (T (list num)))))
  (mhelp "Causes all subformulas at depth greater than n to be printed as &."))

(defun depth (n) (setq printdepth n))

(defmexpr pw
  (argtypes gwff)
  (argnames gwff)
  (arghelp "print gwff")
  (print-command t)
  (mhelp "Print gwff."))

;(defun pw (gwff)
;  (msg F (gwff . gwff)))
;already defined in prtcmd.lisp

(defmexpr pwscope
  (argtypes gwff)
  (argnames gwff)
  (arghelp "print gwff")
  (print-command t)
  (mhelp "print gwff with all brackets restored."))

;(defun pwscope (gwff)
;  (let ((allscopeflag T))
;    (pw gwff)))
;already defined in prtcmd.lisp

(defmexpr pplan
  (argtypes pline)
  (argnames pline)
  (arghelp "print planned line")
  (defaultfns (lambda (pline) (list (pline-default pline))))
  (print-command t)
  (mhelp "	Print a planned line and all its supports."))

(defun pplan (pline)
  (declare (special dproof))
  (prtlines (sort (copy (assoc pline (get dproof 'plans)))
		  #'lineordering)))

(defmexpr pl
  (argtypes posinteger posinteger)
  (argnames num1 num2)
  (arghelp "Lower bound" "Upper bound")
  (defaultfns
    (lambda (num1 num2)
      (cond ((eq num1 '$)
	     (if (eq num2 '$)
		 (list (linealias (car (get dproof 'lines)))
		       (linealias (car (last (get dproof 'lines)))))
		 (list (linealias (car (get dproof 'lines))) num2)))
	    ((eq num2 '$)
	     (list num1 (linealias (car (last (get dproof 'lines))))))
	    (t (list num1 num2)))))
  (closefns prtlines)
  (print-command t)
  (mhelp "	Print all proof lines in a given range."))

(defun pl (num1 num2)
  (remove-if #'(lambda (z)
		 (or (> num1 (linealias z)) (> (linealias z) num2)))
	     (get dproof 'lines)))

(defmexpr pall
  (print-command t)
  (MHELP "	Print all the lines in the current proof outline."))

(defmexpr find-line
  (argnames wff vars meta)
  (argtypes gwff0 gvarlist yesno)
  (arghelp "Wff to look for" "Free variables that may be alphabetically changed"
	   "Treat remaining free vars as subterms?")
  (defaultfns (lambda (wff vars meta) 
		(list wff (if (eq vars '$) (free-vars-of wff) vars)
		      (if (eq meta '$) nil meta))))
  (print-command t)
  (mhelp "Find all lines matching a certain wff, up to
alphabetic change of bound variables and (possibly) 
alphabetic change of a given list of free variables.
Optionally, you can treat the remaining free variables
as matching any given term (as you might do if you were
asserting an axiom).
e.g. (suppose P is an abbreviation or constant):
FIND-LINE \"P a\" () NO       finds all lines that say \"P a\"
FIND-LINE \"P a\" (\"a\") NO    also finds \"P x\" and \"P y\"
FIND-LINE \"P a\" () YES      finds all the above, plus 
                              \"P [COMPOSE f g]\"
FIND-LINE \"a x\" (\"x\") YES   finds all lines of the form
                              \"SOME-TERM some-var\""))

(defun find-line (wff vars meta)
  (if vars
      (let ((fvs (free-vars-of wff))
	    (mwff nil))
	(dolist (fv fvs)
		(if (memq fv vars) (setq wff (mbed-forall fv wff))))
	(when meta (setq mwff (make-wffschema wff)))
	(dolist (line (proof-lines dproof))
		(let* ((assertion (get line 'assertion))
		       (vlist (free-vars-of assertion)))
		  (when (= (length vlist) (length fvs))
			;this prevents us from matching "x" with "forall x . x"
			(dolist (fv (mapcar 'cons fvs vlist))
				;pair up the free vars in this line with those in the given wff
				;relies of free-vars-of always scanning a wff in the same order.
				(if (memq (car fv) vars)
				    ;if the corresponding variable in the given wff can be changed...
				    (setq assertion (mbed-forall (cdr fv) assertion))))
			            ;then bind it in this line, otherwise don't.
			(if meta 
			    (when (%catch% (match mwff assertion) (fail nil)) 
				  (prtline line))
			  (when (wffeq-ab assertion wff) (prtline line)))))))
    (if meta (let ((mwff (make-wffschema wff)))
	       (dolist (line (proof-lines dproof))
		       (when (%catch% (match mwff (get line 'assertion)) (fail nil))
			     (prtline line))))
      (dolist (line (proof-lines dproof))
	      (when (wffeq-ab (get line 'assertion) wff)
		    (prtline line))))))

		  
(defmexpr tableau
  (argtypes posinteger)
  (argnames line)
  (arghelp "Line to start from")
  (defaultfns (lambda (line) (if (eq line '$) 
				 (list (linealias (car (last (get dproof 'lines)))))
			       (list line))))
  (mhelp "Print the part of the proof which justifies the given line,
in a natural deduction tableau format."))

(defun pall () 
  (prfw-pall) (terpri) (prtlines (get dproof 'lines))
  (when (and print-comments (get dproof 'comment) (listp (get dproof 'comment)) (not (memq style '(scribe tex tex-1))))
	(eval (get dproof 'comment))))

;;;called in response to ^P. 

(defmexpr ^p
  (print-command t)
  (mhelp "Print current plan-support pair in the proof."))

(defun ^p () (progn (prfw-^p) (prtactive)))

(defun prtactive ()
  (cond ((not (get dproof 'lines)) (msg "No proof in progress!"))
	((get dproof 'plans) (pplan (caar (get dproof 'plans))))
	(t (pall)))
  (msg T))

(defmexpr ^pn
  (print-command t)
  (mhelp "Print current plan-support pair in the proof, as in ^P, but 
also print just the line numbers of the other lines in the proof."))

(defun ^pn () (progn (prfw-^pn) 
		     (prtactivenos)))

(defun prtactivenos ()
  (let ((lines (get dproof 'lines))
	(plans (get dproof 'plans)))
    (cond ((not lines) (msg "No proof in progress!"))
	  ((not plans) (pall))
	  (t (^pn-recurse lines (car plans))))))

(defun ^pn-recurse (lines plan)
  (if (null lines) (msg t t)
    (progn
      (if (memq (car lines) plan) (progn (prtlines (list (car lines)))
					 (^pn-recurse (cdr lines) plan))
	    (progn (msg "(" (linealias (car lines)))
		   (if (null (cdr lines)) (msg ")" t t) ;no more lines
		     (if (and (= (linealias (cadr lines)) (1+ (linealias (car lines))))
			      (not (memq (cadr lines) plan))) 
			 (find-next-gap (linealias (car lines)) lines plan)
		       (progn (msg ")" t) (^pn-recurse (cdr lines) plan)))))))))

(defun find-next-gap (number lines plan)
  (if (null (cdr lines)) (msg "--" (linealias (car lines)) ")" t t) ;this is the last line
    (if (memq (cadr lines) plan) (progn (msg "--" (linealias (car lines)) ")" t)
					(^pn-recurse (cdr lines) plan)); the next line is a plan
      (if (not (= (linealias (cadr lines)) (1+ number))) 
	  (progn (msg "--" (linealias (car lines)) ")" t)
		 (^pn-recurse (cdr lines) plan))
				;the next line breaks sequence
	(find-next-gap (1+ number) (cdr lines) plan)))))

(defmexpr pline
  (argtypes existing-line)
  (argnames line)
  (arghelp "Line Number")
  (closefns prtline)
  (print-command t)
  (mhelp "	Print a specified line."))

(defun pline (label) label)

(context otl-status)

(defmexpr count-lines
  (mainfns count-lines)
  (print-command t)
  (mhelp "Show the number of lines in the current proof."))

(defun count-lines ()
  (msgf "The current proof contains " (length (proof-lines dproof)) " lines." t))

(defmexpr pstatus
  (mainfns PRTSTATUS)
  (print-command t)
  (mhelp "Give the current status information, i.e. planned lines and their
 supports. If work is being saved, issues an appropriate message."))

(defun prtstatus ()
  (if (get dproof 'lines)
      (if (get dproof 'plans)
	  (dolist (pl (get dproof 'plans))
	    (if (eq style 'scribe)
		(msg "@W{" (mapcar #'linealias pl) "}")
		(msg (mapcar #'linealias pl))))
	  (msg " No planned lines "))
      (msg " No proof in progress "))
  (if (eq style 'scribe) (msg "@*" t) (terpri))
  (if (and saving-work-p (not (eq style 'scribe)))
      (msg T "Saving in file " save-work-file ".")))

(defmexpr subproof
  (argtypes pline)
  (argnames pline)
  (arghelp "Line to prove")
  (defaultfns (lambda (pline) (list (pline-default pline))))
  (closefns cstatus)
  (mhelp "	Concentrate on proving a particular planned line."))

(defun subproof (pline)
  (putprop dproof (cons (assoc pline (get dproof 'plans))
			(remove (assoc pline (get dproof 'plans))
				(get dproof 'plans)))
	   'plans))

(defmexpr sponsor
  (argtypes pline existing-linelist)
  (argnames pline linelist)
  (arghelp "Planned line" "Sponsoring lines")
  (defaultfns (lambda (pline linelist)
		(list (pline-default pline) linelist)))
  (closefns cstatus)
  (mhelp "	Add new sponsoring lines to the sponsors of a planned line."))

(defun sponsor (pline linelist)
  (do ((linelist (nreverse linelist) (cdr linelist))
       (xxx (cdr (assoc pline (get dproof 'plans)))))
      ((null linelist)
       (putprop pline xxx 'support)
       (rplacd (assoc pline (get dproof 'plans) :test #'eq) xxx))
    (if (not (memq (car linelist) xxx))
	(push (car linelist) xxx))))

(defmexpr unsponsor
  (argtypes pline existing-linelist)
  (argnames pline linelist)
  (arghelp "planned line" "Unwanted sponsoring lines")
  (defaultfns
    (lambda (pline linelist)
      (cond ((eq pline '$)
	     (let ((pline (pline-default pline)))
	       (if (eq linelist '$)
		   (list pline (cdr (assoc pline (get dproof 'plans))))
		   (list pline linelist))))
	    ((eq linelist '$)
	     (list pline (cdr (assoc pline (get dproof 'plans)))))
	    (T (list pline linelist)))))	    
  (closefns cstatus)
  (mhelp "      Remove a list of unwanted sponsoring lines from among the 
sponsors of a planned line."))

(defun unsponsor (pline linelist)  
  (let ((xxx (cdr (assoc pline (get dproof 'plans)))))
    (dolist (line linelist
		  (progn (putprop pline xxx 'support)
			 (rplacd (assoc pline (get dproof 'plans) :test #'eq) xxx)))
      (setq xxx (delete line xxx)))))


;;; Added pl* 11NOV90 DAN
(context otl-printing)

(defmexpr pl*
  (argtypes line-range-list)
  (argnames print-ranges)
  (arghelp "ranges to print")
  (defaultfns
   pl*-defaults)
  (mainfns pl*)
  (print-command t)
  (mhelp "Print all proof lines in given ranges."))

(defun pl* (line-ranges)
  (dolist (range line-ranges)
    (prtlines (pl (car range) (cdr range)))))

(defun pl*-defaults (arg)
  (if (and (eq arg '$) (proof-lines dproof))
      (list (list (get-line-range '--)))
    (list arg)))

(defun prfw-pall ()
  (declare (special *using-interface*))
  (when (and proofw-all (streamp *prfw-pall-window*) (open-stream-p *prfw-pall-window*))
	(let ((*standard-output* *prfw-pall-window*)
	      (style (if use-window-style window-style style)))
	  (if *using-interface*
	      (clear-window *prfw-pall-window*)
	    (dotimes (i blank-lines-inserted) (terpri)))
	  (prtlines (get dproof 'lines)) (terpri)
	  (when (and print-comments (get dproof 'comment) (listp (get dproof 'comment)))
		(eval (get dproof 'comment)))
	  (finish-output *standard-output*))))

(defun prfw-^p ()
  (declare (special *using-interface*))
  (when (and proofw-active (streamp *prfw-^p-window*) (open-stream-p *prfw-^p-window*))
	(let ((*standard-output* *prfw-^p-window*)
	      (style (if use-window-style window-style style)))
	  (if *using-interface*
	      (clear-window *prfw-^p-window*)
	    (dotimes (i blank-lines-inserted) (terpri)))
	  (prtactive)
	  (finish-output *standard-output*))))

(defun prfw-^pn ()
  (declare (special *using-interface*))
  (when (and proofw-active+nos (streamp *prfw-^pn-window*) (open-stream-p *prfw-^pn-window*))
	(let ((*standard-output* *prfw-^pn-window*)
	      (style (if use-window-style window-style style)))
	  (if *using-interface*
	      (clear-window *prfw-^pn-window*)
	    (dotimes (i blank-lines-inserted) (terpri)))
	  (prtactivenos)
	  (finish-output *standard-output*))))

(defun tableau (line)
  (let ((outfilename (concatenate 'string "/tmp/foo" 
                                   (princ-to-string (get-universal-time))))
	(reroute-close-message nil)
	(line (numalias line)))
    (reroute-output outfilename *default-pathname-defaults*
		    (tr-print-tableau-main leftmargin rightmargin line))
    (with-open-file (instream outfilename)
      (do ((output (read-line instream nil nil nil) (read-line instream nil nil nil))
	   (stringlist nil))
	  ((null output) (dolist (str stringlist) (msgf str)) (delete-file outfilename))
	  (push output stringlist)))))

(defun tr-print-tableau-main (l r line)
  (let ((center (round (/ (+ l r) 2)))
	(label-width (+ (length (princ-to-string (line-linenumber line))) 2))
	(justwidth (length (princ-to-string (car (line-justification line)))))
	(old-tabstops nil))
    (msg T T (t (- center (round (/ label-width 2)))) "(" (line-linenumber line) ")" t)
    (msg (t (- center (round (/ justwidth 2)))) (car (line-justification line)) t)
    (setq old-tabstops (list (cons center "|")))
    (do ((plist (last (line-justification line)) (rec-tableau-sons plist))
	 (old-plist nil plist))
	((equal plist old-plist) (dolist (tb old-tabstops) (msg (t (car tb)) "|")))
	(if old-plist (setq old-tabstops (rec-tableau-print l r old-plist old-tabstops))))))

(defun rec-tableau-print (l r plist &optional (old-tabstops nil))
  (declare (ignore old-tabstops))
  (let ((tabstops (closeness-filter2 (mapcar #'(lambda (x) (cons (round (car x)) (cdr x)))
					    (sort (calc-tabstops l r plist) 
						  #'(lambda (x y) (< (car x) (car y))))))))
   ; (dolist (tb old-tabstops) (msg (t (car tb)) "|"))
    (msg t)
    (do* ((i 0 (1+ i))
	  (oldshape " " (or newshape " "))
	  (newshape (cdr (assoc i tabstops)) (cdr (assoc i tabstops))))
	 ((> i (caar (last tabstops))))
	 (if newshape (progn 
			(if (> (length newshape) 1) (setq newshape (char newshape 1)))
			(if (string= newshape "\\") (msg "/")
			  (if (string= newshape "/") (msg "\\")
			    (msg newshape))) 
			(if (or (string= newshape "/") (string= newshape "+"))
			    (setq newshape "-")
			  (setq newshape " ")))
	   (progn (if (string= oldshape "-") (setq newshape "-")) (if (string= oldshape "/") (msg "\\")
									 (if (string= oldshape "\\") (msg "/")
									   (msg oldshape))))))
    (msg t)
    (mapcar #'(lambda (x y) (p-tableau-elt x (line-linenumber y))) tabstops (remove-if #'null (flatten-mstlist plist)))
    (msg t)
    (dolist (tb tabstops) (msg (t (car tb)) "|"))
    (msg t)
    (mapcar #'(lambda (x y) (p-tableau-just x (car (line-justification y)))) tabstops (remove-if #'null (flatten-mstlist plist)))
    (msg t)
    tabstops))

(defun closeness-filter2 (alist)
  (if (< (length alist) 2) alist
    (let ((first (car alist))
	  (second (cadr alist)))
      (if (> 9 (- (car second) (car first)))
	  (cons (if (string= (char (cdr first) 0) "*") first (cons (car first) (concatenate 'string "*" (cdr first))))
		(closeness-filter2 (cons (cons (car second) (concatenate 'string "*" (cdr second))) (cddr alist))))
	(if (> 5 (- core:rightmargin (car second)))
	    (cons first (closeness-filter2 (cons (cons (car second) (concatenate 'string "*" (cdr second))) (cddr alist))))
	  (cons first (closeness-filter2 (cdr alist))))))))

(defun p-tableau-elt (tb l)
  (if (string= (char (cdr tb) 0) "*") (msg (t (car tb)) "*")
  (msg (t (- (car tb) (round (/ (1+ (length (princ-to-string l))) 2)))) "(" l ")")))

(defun p-tableau-just (tb l)
  (let* ((to-print (princ-to-string l))
	 (lw (min 8 (length to-print)))
	 (string (if (> (length to-print) lw) (adjust-array to-print lw) to-print))
	 (justwidth (length string)))
    (if (string= (char (cdr tb) 0) "*") (msg (t (car tb)) "*")
      (if (> justwidth lw) (msg (t (car tb)) "*")
	(msg (t (- (car tb) (round (/ justwidth 2)))) string)))))

(defun rec-tableau-sons (plist)
  (if (null plist) nil
    (if (listp (car plist)) (cons (rec-tableau-sons (car plist)) (rec-tableau-sons (cdr plist)))
      (cons (last (line-justification (car plist))) (rec-tableau-sons (cdr plist))))))


;-----------------------------------------------------
;following originally from mtree-print:

(defun contains-lists (e)
  (if (null e) nil
    (if (and (not (null (car e))) (listp (car e)))
	t
      (contains-lists (cdr e)))))


(defun flatten-mstlist (pl)
  (if (null pl) nil
    (if (and (not (null (car pl))) (listp (car pl)))
	(flatten-mstlist (append (car pl) (cdr pl)))
      (cons (car pl) (flatten-mstlist (cdr pl))))))
      
(defun all-nils (plist)
  (if (null plist) t
  (and (null (car plist)) (all-nils (cdr plist)))))


(defun calc-tabstops (l r plist &optional (list-so-far nil))
  (if (or (null plist) (all-nils plist)) list-so-far
    (progn
      (let* ((number (length plist))
	     (block-size (/ (- r l) number)))
	(if (contains-lists plist)
	    (progn
	      (do* ((i l (+ i block-size))
		    (counter 0 (1+ counter))
		    (entry (car plist) (nth counter plist)))
		   ((= counter number) list-so-far)
		   (if (and (not (null entry)) (listp entry) (contains-lists entry))
		       (setq list-so-far (calc-tabstops i (+ i block-size) entry list-so-far))
		     (if (or (all-nils entry) (null entry)) list-so-far
		     (let* ((n2 (length entry)) ;was 1+
			    (b2 (/ block-size n2)))
		       (do ((j (+ i (/ b2 2)) (+ b2 j))
			    (count 1 (1+ count)))
			   ((> count n2) list-so-far)
			   (if (= count 1)
			       (if (= n2 1) (setq list-so-far (cons (cons j "|") list-so-far))
						  (setq list-so-far (cons (cons j "/") list-so-far)))
			     (if (= count n2)
				 (setq list-so-far (cons (cons j "\\") list-so-far)) 
			       (setq list-so-far (cons (cons j "+") list-so-far))))))))))
	  (do ((j (+ l (/ block-size 2)) (+ block-size j))
	       (count 1 (1+ count)))
	      ((> count number) list-so-far)
	      (if (= count 1)
		  (if (= number 1) (setq list-so-far (cons (cons j "|") list-so-far))
					 (setq list-so-far (cons (cons j "/") list-so-far)))
		(if (= count number)
		    (setq list-so-far (cons (cons j "\\") list-so-far)) 
		  (setq list-so-far (cons (cons j "+") list-so-far))))))))))
