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

;;;
;;; File: Linenumber1
;;; Package: OTLNL
;;;
;;; defines functions that  update proof structure, and
;;; provide  defaults.
;;;
;;;LINES are arranged in increasing order.


(part-of otlnl)

(deffile linenumber1
  (part-of otlnl)
  (extension lsp)
  (mhelp "Defines functions which update the proof outline and provide
 defaults for line numbers."))

;;;;Global Variables: 1. dproof 

;;;;Gaps are arranged in decreasing order of the lines constituting the gap.

;;;Use distinct support
;;;lines from the proof structure, unless we run out of 
;;;support lines in which case just use the last support
;;;line. In case the plan line doesn't have any support
;;;lines use a $.

(defflag support-numbers
  (flagtype symbol)
  (default NIL)
  (subjects otl-vars outline)
  (mhelp "This has three possible settings:
GAP: new support lines will be put in the gap between the 
current planned line and the previous line, whatever it is.
PLAN: new support lines will be put immediately after the
previous (lower-numbered) planned line, if there is one 
(and as for NIL if there isn't).
NIL (or anything else): new support lines will be put in 
whatever seems to be a sensible place.

This flag may well be useless (although non-NIL values
will force it to do the right thing, TPS will probably
do the right thing anyway)."))

(defun line-no-defaults-to (default-exist default-new)
  (if default-new
      (let ((xxx
	     (%catch% (line-no-defaults-to-main default-exist default-new)
		      (error expand-catch-throw))))
	(cond ((consp xxx) xxx)
	      ((equal xxx 'no-defaults) default-new)))
      nil))

;;(defun line-no-defaults-to-main (default-exist default-new)
;;  (declare (special gaps))
;;  (let ((min-support-line 0)
;;	(max-plan-line 10000)
;;	(plan-lines-above-default nil)
;;	(incr 0)
;;	(list-of-plan-lines nil)
;;	(list-of-pairs nil)
;;	(new-gaps nil)
;;	(lines-used nil))
;;    (declare (special lines-used))
;;    ;;Extract necessary information from the first argument
;;    (setq default-exist (match-variables default-exist))
;;    (do ((default-exist default-exist (cdr default-exist)))
;;	((null default-exist)) 
;;      (let ((plan-line (caar default-exist)))
;;	(cond ((fixnump plan-line)
;;	       (push plan-line lines-used)
;;	       (setq max-plan-line (min max-plan-line plan-line)))))
;;      (do ((plan-support-pair (cdar default-exist)
;;			      (cdr plan-support-pair)))
;;	  ((null plan-support-pair))
;;	(cond ((fixnump (car plan-support-pair))
;;	       (push (car plan-support-pair) lines-used)
;;	       (setq min-support-line
;;		     (max min-support-line (car plan-support-pair)))))))
;;    (setq min-support-line (1+ min-support-line))
;;    ;;;find number of empty lines between min-support-line and
;;    ;;; max-plan-line
;;    (setq new-gaps (update-gaps1 gaps lines-used))     
;;    (let ((gap-name (find-closest-gap new-gaps max-plan-line)))      
;;      (setq min-support-line
;;	    (max min-support-line
;;		 (1+ (get (get gap-name 'min-label) 'linenumber))))
;;      (setq max-plan-line
;;	    (1- (get (get gap-name 'max-label) 'linenumber)))
;;      (setq incr (- max-plan-line  min-support-line -1)))
;;    (cond ((not (> max-plan-line min-support-line))
;;	   (throw 'error 'no-defaults)))
;;    (do ((default-new default-new (cdr default-new)))
;;	((null default-new))
;;      (do ((line-list (car default-new) (cdr line-list)))
;;	  ((null line-list))
;;	(cond ((fixnump (car line-list))
;;	       (push (car line-list) lines-used)))))
;;  (let ((length (length default-new)))
;;      (setq incr (floor incr length)) ;quotient
;;      (cond ((zerop incr) (throw 'error 'no-defaults)))
;;      (do ((xxx max-plan-line (- xxx incr))
;;	   (num 1 (1+ num)))
;;	  ((> num length)
;;	   (setq list-of-plan-lines (nreverse list-of-plan-lines)))
;;	(push xxx list-of-plan-lines)))
;;    (do ((plan-support-pair (car default-new) (car default-new))
;;	 (default-new (cdr default-new) (cdr default-new))
;;	 (pair-name (gensym) (gensym))
;;	 (next-line 1001 1001)
;;	 (plan-line-used 1000 1000))
;;	((null plan-support-pair))
;;      (push pair-name list-of-pairs)
;;      (cond ((fixnump (car plan-support-pair))
;;	     (cond ((> (car plan-support-pair) (1+ max-plan-line))
;;		    (throw 'error 'no-defaults)))
;;		    ;;(throwfail "Default " (car plan-support-pair)
;;		    ;;" specified for a new plan line is greater than an 
;;		    ;;old plan line " (1+ max-plan-line) ".")
;;	     (setq plan-line-used (car plan-support-pair))
;;	     (let ((closest-plan-line
;;		    (find-closest-plan-line list-of-plan-lines
;;					    plan-line-used)))
;;	       (setq next-line
;;		     (minimize-num (- closest-plan-line incr -1)
;;				   incr plan-lines-above-default 
;;				   plan-line-used min-support-line))
;;	       (cond ((< closest-plan-line plan-line-used)
;;		      (push plan-line-used plan-lines-above-default)))
;;	       (setq list-of-plan-lines
;;		     (delete closest-plan-line list-of-plan-lines)))
;;	     (putprop pair-name
;;		      (find-defaults-for-pair plan-line-used next-line 
;;					      plan-support-pair)
;;		      'represents))
;;	    (T
;;	     ;;Now we'll check if any other defaults have been
;;	     ;;specified, in which case we'll first find the minimum
;;	     ;;line used and then fill other defaults on the basis of
;;	     ;;that. Defaults will be calculated so long as they don't
;;	     ;;overlap, other defaults that  have been specified by
;;	     ;;the user (or calculated on the basis of those), after
;;	     ;;which the others will be left untouched.
;;	     (let ((flag nil)
;;		   (plan-support-pair plan-support-pair))
;;	       (do ((line-num (car plan-support-pair) (car plan-support-pair))
;;		    (plan-support-pair (cdr plan-support-pair)
;;				       (cdr plan-support-pair)))
;;		   ((null plan-support-pair))
;;		 (cond ((and (fixnump line-num) (not (numalias line-num)))
;;			(setq flag T)
;;			(setq next-line (min next-line line-num)))))    
;;	       (cond ((and flag (> next-line (car list-of-plan-lines)))
;;		      (putprop pair-name plan-support-pair 'represents))
;;		     (flag
;;		      (do ((xxx (car list-of-plan-lines)
;;				(car list-of-plan-lines1))
;;			   (list-of-plan-lines1 (cdr list-of-plan-lines)
;;					       (cdr list-of-plan-lines1)))
;;			  ((not (and list-of-plan-lines1
;;				     (> (- xxx next-line) incr)))       
;;			   (setq plan-line-used
;;				 (maximize-num xxx incr lines-used
;;					       max-plan-line))
;;			   (setq list-of-plan-lines
;;				 (delete xxx list-of-plan-lines))	   
;;			   (let ((aaa (cond (list-of-plan-lines1
;;					     (- xxx incr -1))
;;					    (T min-support-line))))
;;			     (setq next-line 
;;				   (minimize-num
;;				    aaa incr plan-lines-above-default
;;				    plan-line-used min-support-line)))))
;;		      (putprop pair-name
;;			       (find-defaults-for-pair
;;				plan-line-used next-line
;;				plan-support-pair) 'represents))
;;		     (T
;;		      (putprop pair-name plan-support-pair 'represents)
;;		      (putprop pair-name T 'process)))))))
;;    (do ((pair-name (car list-of-pairs) (car list-of-pairs))
;;	 (list-of-pairs (cdr list-of-pairs) (cdr list-of-pairs))
;;	 (next-line 0 0))
;;	((null pair-name))
;;      (cond ((get pair-name 'process)
;;	     (let ((plan-support-pair (get pair-name 'represents))
;;		   (plan-line-used
;;		    (maximize-num (car list-of-plan-lines) incr lines-used
;;				  max-plan-line)))
;;	       (setq next-line 
;;		     (minimize-num  (- (car list-of-plan-lines) incr -1) incr
;;				   plan-lines-above-default plan-line-used
;;				   min-support-line))
;;	       (setq list-of-plan-lines (cdr list-of-plan-lines))
;;	       (putprop pair-name
;;			       (find-defaults-for-pair
;;				plan-line-used next-line
;;				plan-support-pair) 'represents)))))
;;    (let ((return-list nil))
;;      (do ((list-of-pairs list-of-pairs (cdr list-of-pairs)))
;;	  ((null list-of-pairs))
;;	(push (get (car list-of-pairs) 'represents) return-list))
;;      return-list)))

;;; Removed gaps as a special variable. -- 6/19/87 DAN
(defun line-no-defaults-to-main (default-exist default-new)
  (let ((min-support-line 0)
	(max-plan-line (if (get dproof 'linealiases)
			   (reduce 'max (mapcar 'car (get dproof 'linealiases)))
			 1000000))
	(plan-lines-above-default nil)
	(incr 0)
	(list-of-plan-lines nil)
	(list-of-pairs nil)
	(new-gaps nil)
	(lines-used nil))
    (declare (special lines-used))
    ;;Extract necessary information from the first argument
    (setq default-exist (match-variables default-exist))
    (dolist (default default-exist)
      (let ((plan-line (car default)))
	(when (integerp plan-line)
	  (push plan-line lines-used)
	  (setq max-plan-line (min max-plan-line plan-line))))
      (dolist (plan-support-pair (cdr default))
	(when (integerp  plan-support-pair)
	  (push plan-support-pair lines-used)
	  (setq min-support-line
		(max min-support-line  plan-support-pair)))))
    (if (eq support-numbers 'core::plan)
	(let* ((planlist (mapcar 'linealias (mapcar 'car (get dproof 'plans)))) ;from the list of plan numbers...
	       (planlist (if (null planlist) nil
			   (remove-if #'(lambda (x) (>= x max-plan-line)) planlist))) ; remove all those above max-p-l
	       (planlist (if (null planlist) min-support-line
			   (reduce 'max planlist)))) ; and pick the biggest of what's left
	  (setq min-support-line planlist)) ; which is a number, not a list, by this point.
      (if (eq support-numbers 'core::gap)
	  (do ((number (1- max-plan-line) (1- number)))
	      ((or (eq number 0) (numalias number)) (setq min-support-line number)))))
    (incf min-support-line)
    ;;;find number of empty lines between min-support-line and
    ;;; max-plan-line
    (setq new-gaps (update-gaps1 (gaps) lines-used))     
    ;; Added checks for null gap DAN 12Mar88
    (let ((gap-name (or (find-closest-gap new-gaps max-plan-line)
			(throw 'error 'no-defaults))))      
      (if (integerp (get (get gap-name 'min-label) 'linenumber))
	  (setq min-support-line
		(max min-support-line
		     (1+ (get (get gap-name 'min-label) 'linenumber))))
	(throw 'error 'no-defaults))
      (if (integerp (get (get gap-name 'max-label) 'linenumber))
	  (setq max-plan-line
		(1- (get (get gap-name 'max-label) 'linenumber)))
	(throw 'error 'no-defaults))
      (setq incr (- max-plan-line  min-support-line -1)))
    (unless (> max-plan-line min-support-line)
      (throw 'error 'no-defaults))
    (dolist (default default-new)
      (dolist (line default)
	(when (integerp line)
	  (push line lines-used))))
    (let ((length (length default-new)))
      (setq incr (floor incr length))	;quotient
      (when (zerop incr) (throw 'error 'no-defaults))
      (do ((xxx max-plan-line (- xxx incr))
	   (num 1 (1+ num)))
	  ((> num length)
	   (setq list-of-plan-lines (nreverse list-of-plan-lines)))
	(push xxx list-of-plan-lines)))
    (do ((plan-support-pair (car default-new) (car default-new))
	 (default-new (cdr default-new) (cdr default-new))
	 (pair-name (gensym) (gensym))
	 (next-line 1001 1001)
	 (plan-line-used 1000 1000))
	((null plan-support-pair))
      (push pair-name list-of-pairs)
      (cond ((integerp (car plan-support-pair))
	     (if (> (car plan-support-pair) (1+ max-plan-line))
		 (throw 'error 'no-defaults))
	     ;;(throwfail "Default " (car plan-support-pair)
	     ;;" specified for a new plan line is greater than an 
	     ;;old plan line " (1+ max-plan-line) ".")
	     (setq plan-line-used (car plan-support-pair))
	     (let ((closest-plan-line
		    (find-closest-plan-line list-of-plan-lines
					    plan-line-used)))
	       (setq next-line
		     (minimize-num (- closest-plan-line incr -1)
				   incr plan-lines-above-default 
				   plan-line-used min-support-line))
	       (if (< closest-plan-line plan-line-used)
		   (push plan-line-used plan-lines-above-default))
	       (setq list-of-plan-lines
		     (delete closest-plan-line list-of-plan-lines)))
	     (putprop pair-name
		      (find-defaults-for-pair plan-line-used next-line 
					      plan-support-pair)
		      'represents))
	    (T
	     ;;Now we'll check if any other defaults have been
	     ;;specified, in which case we'll first find the minimum
	     ;;line used and then fill other defaults on the basis of
	     ;;that. Defaults will be calculated so long as they don't
	     ;;overlap, other defaults that  have been specified by
	     ;;the user (or calculated on the basis of those), after
	     ;;which the others will be left untouched.
	     (let ((flag nil)
		   (plan-support-pair plan-support-pair))
	       (do ((line-num (car plan-support-pair) (car plan-support-pair))
		    (plan-support-pair (cdr plan-support-pair)
				       (cdr plan-support-pair)))
		   ((null plan-support-pair))
		 (when (and (integerp line-num) (not (numalias line-num)))
		   (setq flag T)
		   (setq next-line (min next-line line-num))))    
	       (cond ((and flag (> next-line (car list-of-plan-lines)))
		      (putprop pair-name plan-support-pair 'represents))
		     (flag
		      (do ((xxx (car list-of-plan-lines)
				(car list-of-plan-lines1))
			   (list-of-plan-lines1 (cdr list-of-plan-lines)
						(cdr list-of-plan-lines1)))
			  ((not (and list-of-plan-lines1
				     (> (- xxx next-line) incr)))       
			   (setq plan-line-used
				 (maximize-num xxx incr lines-used
					       max-plan-line))
			   (setq list-of-plan-lines
				 (delete xxx list-of-plan-lines))	   
			   (let ((aaa (if list-of-plan-lines1
					  (- xxx incr -1)
					  min-support-line)))
			     (setq next-line 
				   (minimize-num
				    aaa incr plan-lines-above-default
				    plan-line-used min-support-line)))))
		      (putprop pair-name
			       (find-defaults-for-pair
				plan-line-used next-line
				plan-support-pair) 'represents))
		     (T
		      (putprop pair-name plan-support-pair 'represents)
		      (putprop pair-name T 'process)))))))
    (dolist (pair-name list-of-pairs)
      (when (get pair-name 'process)
	(let ((next-line 0)
	      (plan-support-pair (get pair-name 'represents))
	      (plan-line-used
	       (maximize-num (car list-of-plan-lines) incr lines-used
			     max-plan-line)))
	  (setq next-line 
		(minimize-num  (- (car list-of-plan-lines) incr -1) incr
			       plan-lines-above-default plan-line-used
			       min-support-line))
	  (setq list-of-plan-lines (cdr list-of-plan-lines))
	  (putprop pair-name
		   (find-defaults-for-pair
		    plan-line-used next-line plan-support-pair)
		   'represents))))
    (nreverse
     (mapcar #'(lambda (label) (get label 'represents)) list-of-pairs))))


(defun find-defaults-for-pair (plan-line-used next-line plan-support-pair)
  (declare (special lines-used))
  (if (or (eq plan-line-used '$) (> next-line plan-line-used))
      plan-support-pair
      (let ((new-pair (if (eq (car plan-support-pair) '$)
			  (progn
			   (push plan-line-used lines-used)
			   (list plan-line-used))
			  (list (car plan-support-pair))))
	    (done-flag nil))
	(dolist (line-num (cdr plan-support-pair) (nreverse new-pair))
	  (cond (done-flag (push line-num new-pair))
		((eq line-num '$)
		 (push next-line lines-used)
		 (push next-line new-pair)
		 (setq next-line
		       (find-next-line next-line lines-used plan-line-used))
		 (if (> (+ 2 next-line) plan-line-used)
		     (setq done-flag T)))
		(T (push line-num new-pair)))))))

(defun find-next-line (next-line lines-used plan-line-used)
  (do ((xxx (1+ next-line) (1+ xxx)))
      ((or (not (member xxx lines-used)) (> (+ 2 xxx) plan-line-used))
       (if (> (+ 2 xxx) plan-line-used) (+ 2 xxx) xxx))))

(defun match-variables (default-exist)
  (declare (special dproof))
  (let ((new-defaults nil)
	(plans (mapcar #'(lambda (xxx) (mapcar #'linealias xxx))
		       (get dproof 'plans))))
    (dolist (default default-exist new-defaults)
	    (if (integerp (car default))
		(push default new-defaults)
	      (do ((plans plans (cdr plans))
		   (flag nil))
		  ((or flag (null plans))
		   (if (not flag) (throw 'error 'no-defaults)))
		  (setq flag T)
		  (let ((new-lines nil))
		    (dolist (line-num default
				      (push (append (car plans) new-lines)
					    new-defaults))
			    (when (integerp line-num)
				  (cond ((or (not (< line-num (caar plans)))
					     (and (numalias line-num)
						  (not (memq line-num
							     (car plans)))))
					 (setq flag nil)
					 (return))
					((not (numalias line-num))
					 (push line-num new-lines)))))))))))

;(defun maximize-num (plan-line-used incr lines-used max-plan)
;  (if (member plan-line-used lines-used)
;      '$				;modify this later.
;      (let ((limit (min (+ plan-line-used incr) max-plan)))
;	(do ((yyy (1+ plan-line-used) (1+ yyy)))
;	    ((or (member yyy lines-used) (> yyy limit))
;	     (if (not (< yyy limit)) plan-line-used
;		 (1- yyy)))))))

(defun maximize-num (plan-line-used incr lines-used max-plan)
  (if (member plan-line-used lines-used)
	;;;'$ ;modify this later.
      plan-line-used
      (let ((limit (min (+ plan-line-used incr) max-plan)))
	(do ((yyy (1+ plan-line-used) (1+ yyy)))
	    ((or (member yyy lines-used)
		 (> yyy limit))
	     (if (< yyy limit) (1- yyy) plan-line-used))))))

(defun minimize-num (line-num incr plan-lines-above-default plan-line-used
			      min-support)
  (declare (special lines-used))
  (if (member line-num lines-used)	 
      (find-next-line line-num lines-used plan-line-used)
      (let ((flag nil))
	(dolist (xxx plan-lines-above-default)
	  (when (or (null xxx) (< xxx line-num)
		    (< (- xxx line-num) (floor (1+ incr) 2)))
	    (if (and xxx (> xxx line-num)) (setq flag xxx))
	    (return)))
	(if flag (1+ flag)
	    (let ((limit (max (- line-num incr -1) min-support)))
	      (do ((xxx (1- line-num) (1- xxx)))
		  ((or (member xxx lines-used) (numalias xxx)
		       (< xxx limit))
		   (cond ((< xxx min-support) min-support)
			 ((< xxx limit) line-num)
			 (T (1+ xxx))))))))))

(defun find-closest-plan-line (list-of-plan-lines plan-line-used)
  (cond ((member plan-line-used list-of-plan-lines) plan-line-used)
	((or (= (length list-of-plan-lines) 1)
	     (not (< plan-line-used (car list-of-plan-lines))))
	 (car list-of-plan-lines))
	((not (> plan-line-used (car (last list-of-plan-lines))))
	 (car (last list-of-plan-lines)))
	(T (do ((list-of-plan-lines list-of-plan-lines
				    (cdr list-of-plan-lines)))
	       ((and (> (car list-of-plan-lines) plan-line-used)
		     (< (cadr list-of-plan-lines) plan-line-used))
		(if (< (- (car list-of-plan-lines) plan-line-used)
		       (- plan-line-used (cadr list-of-plan-lines)))
		    (car list-of-plan-lines)
		    (cadr list-of-plan-lines)))))))

(defun find-defaults (current-plan plan-support-pair)
  (let ((xxx (if (and (eq (car plan-support-pair) '$) current-plan)
		 (list (linealias (car current-plan)))
		 (list (car plan-support-pair)))))
    (do ((current-plan1 (cdr current-plan) (cdr current-plan1))
	 (yyy nil))
	((null current-plan1)(setq current-plan (nreverse yyy)))
      (unless (member (linealias (car current-plan1)) plan-support-pair)
	(push (car current-plan1) yyy)))
    (dolist (yyy (cdr plan-support-pair))
      (if (and (eq yyy '$) current-plan)
	  (progn
	    (push (linealias (car current-plan)) xxx)
	    (setq current-plan (cdr current-plan)))
	  (push yyy xxx)))
    (nreverse xxx)))

(defun line-no-defaults-from (default-exist)
  (declare (special dproof))
  (let ((plans (get dproof 'plans))
	(def-exists nil))
    (do ((plan-support-pair (car default-exist) (car default-exist))
	 (default-exist (cdr default-exist) (cdr default-exist))
	 (xxx nil nil)) ;;xxx is used to keep track of the defaults figured.
	((null plan-support-pair))
      (cond ((eq (car plan-support-pair) '$)
	     ;;if we need the defaults for a plan line , than
	     ;;use the current plan line.
	     (setq xxx (find-defaults (car plans) plan-support-pair)))
	    ((symbolp (car plan-support-pair))
	     (let ((yyy (%catch% (match-variables (list plan-support-pair))
				 (error expand-catch-throw))))
	       (if (consp yyy)
		   (setq xxx (find-defaults (mapcar #'numalias (car yyy))
					    plan-support-pair))
		   (setq xxx plan-support-pair))))
	    ((plusp (car plan-support-pair))
	     (if (assoc (numalias (car plan-support-pair)) plans)
		 (setq xxx (find-defaults
			    (assoc (numalias (car plan-support-pair)) plans)
			    plan-support-pair))
		 (setq xxx plan-support-pair))))
      (push xxx def-exists))
    (nreverse def-exists)))

(defun update-gaps1 (gaps linelist)
  (let ((lines nil))
    (dolist (xxx linelist)
      (if (and (integerp xxx) (not (numalias xxx)) (not (member xxx lines)))
	  (push xxx lines)))
    (setq linelist (sort lines #'>)))
  (let ((new-gaps2 nil))
    (do ((line (car linelist) (car linelist))
	 (linelist (cdr linelist) (cdr linelist))
	 (atom1 (gensym) (gensym))
	 (new-gaps gaps))
	((null line) (append (nreverse new-gaps2) new-gaps))
      (do ((atom (car new-gaps) (car new-gaps1))
	   (new-gaps1 (cdr new-gaps) (cdr new-gaps1)))
	  ((or (null atom)
	       (< (get (get atom 'min-label) 'linenumber) line))
	   (cond ((null atom) nil)
		 ((> (get (get atom 'max-label) 'linenumber) line)
		  (let ((xxx (gentemp)))
		    (putprop xxx line 'linenumber)
		    (cond
		     ((=  (1- (get (get atom 'max-label) 'linenumber))
			  (1+ (get (get atom 'min-label) 'linenumber))))
		     ((= line (1- (get (get atom 'max-label)
				       'linenumber)))
		      (push atom1 new-gaps1)
		      (putprop atom1 (get atom 'min-label) 'min-label)
		      (putprop atom1 xxx 'max-label))
		     ((= line (1+ (get (get atom 'min-label)
				       'linenumber)))
		      (push atom1 new-gaps1)
		      (putprop atom1 (get atom 'max-label) 'max-label)
		      (putprop atom1 xxx 'min-label))
		     (T (let ((yyy (gensym)))
			  (push atom1 new-gaps2)
			  (push yyy new-gaps1)
			  (putprop yyy xxx 'max-label)
			  (putprop yyy (get atom 'min-label) 'min-label)
			  (putprop atom1 (get atom 'max-label) 'max-label)
			  (putprop atom1 xxx 'min-label))))))
		 (T (push atom new-gaps1)))
	   (setq new-gaps new-gaps1))
    (push atom new-gaps2)))))

(defun update-gaps (gaps linelist)
  (let ((lines nil))
    (dolist (xxx linelist)
      (if (get xxx 'linenumber) (push xxx lines)))
    (setq linelist (sort lines #'> :key #'(lambda (label)
					    (get label 'linenumber)))))
  (let ((new-gaps2 nil))
    (do ((line (get (car linelist) 'linenumber)
	       (get (car linelist) 'linenumber))
	 (new-gaps gaps)
	 (linelist (cdr linelist) (cdr linelist)))
	((null line) (append (nreverse new-gaps2) new-gaps))
      (do ((atom (car new-gaps) (car new-gaps1))
	   (new-gaps1 (cdr new-gaps) (cdr new-gaps1)))
	  ((or (null atom)
	       (< (get (get atom 'min-label) 'linenumber) line))
	   (cond ((null atom) nil)
		 ((> (get (get atom 'max-label) 'linenumber) line)
		  (let ((xxx (numalias line)))
		    (cond
		     ((=  (1- (get (get atom 'max-label) 'linenumber))
			  (1+ (get (get atom 'min-label) 'linenumber))))
		     ((= line (1- (get (get atom 'max-label)
				       'linenumber)))
		      (push atom new-gaps1)
		      (putprop atom xxx 'max-label))
		     ((= line (1+ (get (get atom 'min-label)
				       'linenumber)))
		      (push atom new-gaps1)
		      (putprop atom xxx 'min-label))
		     (T (let ((yyy (gensym)))
			  (push atom new-gaps2)
			  (push yyy new-gaps1)
			  (putprop yyy xxx 'max-label)
			  (putprop yyy (get atom 'min-label)
				   'min-label)
			  (putprop atom xxx 'min-label))))))
		 (T (push atom new-gaps1)))
	   (setq new-gaps new-gaps1))
    (push atom new-gaps2)))))

;;(defun find-all-gaps ()
;;  (declare (special dproof gaps))
;;  (setq gaps nil)
;;  (cond ((= (length (get dproof 'lines)) 1)
;;	     (let ((gap-name (gensym)))
;;	       (setq gaps (list gap-name))
;;	       (putprop gap-name (car (get dproof 'lines))
;;			'max-label)
;;	       (putprop gap-name (gentemp) 'min-label)
;;	       (putprop (get gap-name 'min-label) 0 'linenumber)))
;;	(T
;;	 (let ((lines (reverse (get dproof 'lines))))
;;	   (do ((line-num (get (car lines) 'linenumber) next-line-num)
;;		(next-line-num (get (cadr lines) 'linenumber)
;;			       (get (car lines) 'linenumber))
;;		(lines (cddr lines) (cdr lines)))
;;	       ((null next-line-num))
;;	     (cond ((not (=  line-num (1+ next-line-num)))
;;		    (let ((gap-name (gensym)))
;;		      (putprop gap-name (numalias line-num) 'max-label)
;;		      (putprop gap-name (numalias next-line-num) 'min-label)
;;		      (push gap-name gaps)))))
;;	   (cond ((> (get (car (last lines)) 'linenumber) 1)
;;		  (let ((gap-name (gensym)))
;;		    (putprop gap-name (car (last lines)) 'max-label)
;;		    (push gap-name gaps)			    
;;		    (putprop gap-name (gentemp) 'min-label)
;;		    (putprop (get gap-name 'min-label) 0 'linenumber)))))))
;;  (setq gaps (nreverse gaps)))

;;;Modifies GAPS, a property of dproof
;;; Removed gaps as a special variable -- 6/19/87 DAN
;;; Changed gensyms to start new symbols with a "G" -- 7/17/87 DAN

(defun find-all-gaps ()
  (setf (gaps) nil)
  (if (cdr (get dproof 'lines))
      (let ((lines (reverse (get dproof 'lines))))
	(do ((line-num (get (car lines) 'linenumber) next-line-num)
	     (next-line-num (get (cadr lines) 'linenumber)
			    (get (car lines) 'linenumber))
	     (lines (cddr lines) (cdr lines)))
	    ((null next-line-num))
	  (unless (=  line-num (1+ next-line-num))
	    (let ((gap-name (gensym "G")))
	      (putprop gap-name (numalias line-num) 'max-label)
	      (putprop gap-name (numalias next-line-num) 'min-label)
	      (setf (gaps) (push gap-name (gaps))))))
	(when (> (get (car (last lines)) 'linenumber) 1)
	  (let ((gap-name (gensym "G")))
	    (putprop gap-name (car (last lines)) 'max-label)
	    (setf (gaps) (push gap-name (gaps)))		    
	    (putprop gap-name (gentemp) 'min-label)
	    (putprop (get gap-name 'min-label) 0 'linenumber))))
      (let ((gap-name (gensym "G")))
	(setf (gaps) (list gap-name))
	(putprop gap-name (car (get dproof 'lines)) 'max-label)
	(putprop gap-name (gentemp) 'min-label)
	(putprop (get gap-name 'min-label) 0 'linenumber)))
  (setf (gaps) (nreverse (gaps))))

(defun find-closest-gap (gaps line-num)
  (if (symbolp line-num)
      (setq line-num (get line-num 'linenumber)))
  (do ((gaps gaps (cdr gaps)))
      ((or (null (cdr gaps))
	   (< (get (get (car gaps) 'min-label) 'linenumber) line-num))
       (car gaps))))

;;;This function takes gap-name as the first argument and returns the next
;;;available gap whose length is at least as large as the second
;;;argument.

(defun find-previous-gap (gaps gap-name length)
  (do ((listofgaps (cdr (member gap-name gaps)) (cdr listofgaps)))
      ((or (null listofgaps)
	   (> (- (get (get (car listofgaps) 'max-label) 'linenumber)
		 (get (get (car listofgaps) 'min-label) 'linenumber))
	      length))
       (car listofgaps))))


;;(defun remove-unnecessary-gaps ()
;;  (declare (special gaps dproof))
;;  (let ((new-gaps nil)
;;	(lines (get dproof 'linealiases)))
;;    (do ((gap-name (car (last gaps)) (car gaps))
;;	 (gaps (cdr (reverse gaps)) (cdr gaps)))
;;	((null gap-name))
;;      (cond ((planp (get gap-name 'max-label))
;;	     (push gap-name new-gaps))
;;	    (T
;;	     (do ((line-num (get (get gap-name 'max-label) 'linenumber)
;;			    (1+ line-num))
;;		  (xxx nil)
;;		  (new-line-num
;;		   (1+ (get (get gap-name 'min-label) 'linenumber))
;;		   (1+ new-line-num)))
;;		 ((not (numalias line-num))
;;		  (putprop (car gaps) xxx 'min-label))
;;	       (setq xxx (numalias line-num))
;;	       (setq lines (delete (cons line-num xxx) lines
;;				   :test #'equal))
;;	       (push (cons new-line-num xxx) lines)
;;	       (putprop xxx new-line-num 'linenumber)))))
;;    (putprop dproof lines 'linealiases)
;;    (setq gaps new-gaps)))

;;This function removes all unnecessary gaps from the proof structure
;;by RENUMBERING appropriate LINES. It also modifies the property
;;GAPS of DPROOF. 
;;Removed gaps as a special variable -- 6/19/87 DAN

(defun remove-unnecessary-gaps ()
  (let ((new-gaps nil)
	(lines (proof-linealiases dproof))
	(gaps* (gaps)))  ; gaps* is temp var to hold initial value of gaps
    (dolist (gap-name (reverse gaps*))
      (if (planp (get gap-name 'max-label))
	  (push gap-name new-gaps)
	  (do ((line-num (get (get gap-name 'max-label) 'linenumber)
			 (1+ line-num))
	       (xxx nil)
	       (new-line-num (1+ (get (get gap-name 'min-label) 'linenumber))
			     (1+ new-line-num)))
	      ((not (numalias line-num)) (putprop (car (gaps)) xxx 'min-label))
	    (setq xxx (numalias line-num))
	    (setq lines (delete line-num lines :key #'car))
	    (setq lines
		  (do* ((remaining lines (cdr remaining))
			(previous nil (cons first previous))
			(first (car lines) (car remaining)))
		       ((or (null first) (> (car first) new-line-num))
			(nconc (nreverse previous)
			       (cons (cons new-line-num xxx) remaining)))))
;*;	       (push (cons new-line-num xxx) lines)
;*;	       (putprop xxx new-line-num 'linenumber)
	    (setf (line-linenumber xxx) new-line-num))))
;*; (putprop dproof lines 'linealiases)
    (setf (proof-linealiases dproof) lines)
    (setf (gaps) new-gaps)))


(defun introduce-new-plan-line (new-plan-lines old-plan)
  (declare (special listofnewlines))
  (let ((new-plan-structure nil))
    (dolist (new-plan-line new-plan-lines new-plan-structure)
      (unless (member (cons (get new-plan-line 'linenumber) new-plan-line)
		      listofnewlines :test #'equal)
	(push (cons (get new-plan-line 'linenumber) new-plan-line)
	      listofnewlines))
      (push (cons new-plan-line
		  (find-support-lines (get new-plan-line 'linenumber)
				      nil (cdr old-plan)))
	    new-plan-structure))))

(defun set-intersect-p (set1 set2)
  (dolist (elt set1 nil)
	  (if (memq elt set2) (return t))))

