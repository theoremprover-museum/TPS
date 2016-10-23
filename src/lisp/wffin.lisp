;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFF-PARSE)

(deffile wffin
  (part-of wff-parse)
  (extension lsp)
  (mhelp "Contains the parsing function for GENERIC terminal input."))

(context wff-parsing)

(defvar stoplist '(#\Space  #\[ #\] #\( #\) #\. #\; #\, #\: #\` #\~
			    #\Newline #\Tab))

(defvar spacelist '(#\Space #\Newline #\Tab))

(defun char-member (elt list)
  (member elt list :test #'char=))

(defun empty-fill-string ()
  (make-array '(1) :element-type 'tps-string-char :fill-pointer 0
	      :adjustable t))

(defun single-string (char)
  (make-string 1 :initial-element char))

(defun intern-char (char)
  (intern (single-string char) *user*))

(defun intern-str (string)
  (intern (coerce string 'simple-string) *user*))

(defun rd-string (string)
  (if (string= string "")
      (throwfail "The empty string \"\" is not a legal wff."))
  (lexlist-parse (lexscan string)))

(defun get-stringtype (bytestream)
  (declare (special bytestream))
  (let ((index 0) (maxindex (length bytestream)))
    (declare (special index maxindex))
    (%catch% (parse-type)
	   (fail (complain f expand-catch-throw)))))

(defun lexlist-parse (lexlist)
  (declare (special lexlist))
  (let ((typeassoc nil))
    (declare (special typeassoc))
    (finalscan (infixscan (groupscan)))))

(defun groupscan ()
  (declare (special lexlist typeassoc))
  (prog (temp element la type)
   l (cond ((null lexlist) (return (nreverse temp))))
    (setq element (pop lexlist))
    (cond ((eq element ']) (return (nreverse temp)))
	  ((eq element '[)
	   (setq temp (cons (groupscan) temp))
	   (go l))
	  ((eq element 'dot)
	   (return (nreverse (cons (groupscan) temp))))
	  ((not (atom element))
	   (push element lexlist)
	   (throwfail "GroupScan Error - Type symbol " (element . typesym)
		      " probably misplaced."))
	  ((and (symbolp element) (get element 'meta-label))
	   ;; Did we have a quoted token?
	   (if (get element 'lexlist)
	       (let ((lexlist (get element 'lexlist)))
		 (declare (special lexlist))
		 (putprop element (groupscan) 'lexlist)
		 (push element temp))
	       (push element temp))
	   (go l)))
    ;; name root possibly followed by type symbol
    (cond ((atom (car lexlist))
	   (cond ((or (pmprsym-p element) (pmabbsym-p element))
		  (setq la (gensym))
		  (putprop la element 'stands-for)
		  (putprop la (get element 'infix) 'infix)
		  (push la temp))
		 ((or (get element 'printnotype) (abbrev-p element)
		      (label-p element))
		  (push element temp))
		 ((setq la (cdr (assoc element typeassoc)))
		  (push la temp))
		 ((binder-q element)
		  (push element temp))
		 (t (remprop element 'type) (push element temp))))
	  (t (setq type (if (cdar lexlist) (car lexlist) (caar lexlist)))
	     (cond ((or (pmprsym-p element) (pmabbsym-p element))
		    (setq la (gensym))
		    (putprop la element 'stands-for)
		    (putprop la (get element 'infix) 'infix)
		    (putprop la type 'type)
		    (push la temp))
		   ((or (get element 'printnotype) (abbrev-p element)
			(label-p element))
		    (if (equal-type-p type (get element 'type))
			(push element temp)
			(throwfail
			 "GroupScan Error - Giving type-free symbol "
			 (element . gwff)
			 " a wrong type: " (type . typesym) " instead of "
			 ((get element 'type) . typesym) ".")))
		   ((binder-q element)
		    (throwfail "GroupScan Error - Giving binder "
			       (element . fsym)
			       " a type [ " (type . typesym) " ]."))
		   (t (setq la (create-propsym element type))
		      (push (cons element la) typeassoc)
		      ;;(putprop la type 'type)
		      (push la temp)))
	     (pop lexlist)))
    (go l)))

(defun infixscan (grouplist)
  (prog (s pop hold termlist stack)
   next ;;(msg "Next item : " (car grouplist)
    ;;   t "Stack is now : " stack t "On hold : " hold t "Termlist : "
    ;;   termlist "   Proceed" (e (linereadp)))
    (when (null grouplist)
      (when hold
	(setq termlist (cons (maketerm (nreverse hold) nil) termlist))
	(setq hold nil))
      (go finish))
    (setq s (pop grouplist))
    (cond ((consp s) (setq s (infixscan s)))
	  ((binder-p s)
	   (push (pop grouplist) s)
	   (go l))
	  ((get s 'meta-label)
	   ;; Now we need to infixscan each of the arguments.
	   (do ((arguments (get s 'lexlist) (cdr arguments))
		(scanned nil
			 (if (and (symbolp (car arguments))
				  (get (car arguments) 'meta-label)
				  (not (get (car arguments) 'lexlist)))
			     (cons (car arguments) scanned)
			     (cons (infixscan (list (car arguments)))
				   scanned))))
	       ((null arguments)
		(putprop s (nreverse scanned) 'lexlist)))
	   (go addhold))
	  ((get s 'infix) (go l))
	  ((get s 'prefix) (go l)))
   addhold (push s hold)
    (go next)
   l (cond ((and (symbolp s) (get s 'infix))
	    (if hold (push (maketerm (reverse hold) nil) termlist)
		(go addhold)))
	   (hold (throwfail "Illegal grouping : "
			    ((maketerm (reverse hold) nil) . gwff) " precedes "
			    ((if (consp s) (cons s 'scope) s) . gwff) ".")))
    (setq hold nil)
   stack (cond ((null stack) (setq stack (list s)) (go next)))
    (setq pop (pop stack))
    (cond ((and (not (< (bind-priority pop) (bind-priority s)))
		(or (and (symbolp pop) (get pop 'infix))
		    (and (symbolp s) (get s 'infix))))
	   (setq termlist
		 (if (and (symbolp pop) (get pop 'infix))
		     (cons (acons pop (cadr termlist) (car termlist))
			   (cddr termlist))
		     (acons pop (car termlist) (cdr termlist))))
	   (go stack))
	  (t (setq stack (cons s (cons pop stack))) (go next)))
   finish (if stack
	      (progn
	       (setq pop (pop stack))
	       (setq termlist
		     (if (and (symbolp pop) (get pop 'infix))
			 (if (cdr termlist)
			     (cons (cons (cons pop (cadr termlist))
					 (car termlist)) (cddr termlist))
			     (cons (cons (car termlist) pop) (cdr termlist)))
			 (cons (cons pop (car termlist)) (cdr termlist))))
	       (go finish))
	      (return (car termlist)))))




;;;
;;;(DC MAKETERM) 
;;;(MakeTerm WFF ArgList)   [Expr] 
;;; 
;;;Applies to the term in (CAR HOLD) the aruments in (CDR HOLD).  Essential 
;;;the same as LTerm.  At the top level, ANS should be NIL.
;;;

(defun maketerm (hold ans)
  (cond ((null hold) ans)
	((null ans) (maketerm (cdr hold) (car hold)))
	(t (maketerm (cdr hold) (cons ans (car hold))))))

;;;
;;;(DC LEXSCAN) 
;;;
;;;(LEXSCAN BYTESTREAM)   [EXPR]
;;;BYTESTREAM             [SPECIAL]
;;;
;;;Lexical scanning will return a list whose elements are either atoms or 
;;;lists. The atoms are either "[", "]", "." or nameroots.  The lists are 
;;;typesymbols.
;;;

(defun lexscan (bytestream)
  (declare (special bytestream))
  (let ((index 0) (maxindex (length bytestream)))
    (declare (special index maxindex))
    (lexscan-sp)))

#+comment(defun lexscan-sp ()
  (declare (special bytestream index maxindex))
  (prog (nameroot temp type)
   l (cond ((or (= index maxindex) (char= (schar bytestream index) #\)))
	    (return (reverse temp)))
	   ((char-member (schar bytestream index) spacelist) (go next))
	   ((char= (schar bytestream index) #\`)
	    ;; A Backquote introduces a token which is not a wff or type,
	    ;; but may be an arbitrary LISP expression.
	    (push (lexscan-quoted-token) temp)
	    (go l))
	   ((char= (schar bytestream index) #\()
	    (incf index)
	    (setq type
		  (%catch% (parse-type)
			   (fail (throwfail "Type Scan Error - Probably () in type."))))
	    (setq temp (cons (if (atom type) (list type) type) temp))
	    (go l))
	   ((char-member (schar bytestream index) stoplist)
	    (push (if (char= (schar bytestream index) '#\~) 'not
		      (if (char= (schar bytestream index) '#\.) 'dot
			  (intern-char (schar bytestream index))))
		  temp)
	    (go next)))
    ;; the beginning of a nameroot
    (setq nameroot (empty-fill-string))
   nr (cond ((or (= index maxindex)
		 (char-member (schar bytestream index) stoplist))
	     (let ((all-upcase (intern (string-upcase nameroot) *user*)))
	       (if (get all-upcase 'fo-single-symbol)
		   (push all-upcase temp)
		   (if first-order-mode-parse
		       ;; Next line is incorrect.
		       (if lowercaseraise
			   (dotimes (i (length all-upcase))
			     (push (intern-char (char all-upcase i)) temp))
			   (dotimes (i (length nameroot))
			     ;; schar does not work in line below because
			     ;; nameroot is not simple string
			     (push (intern-char (aref nameroot i)) temp)))
		       (if lowercaseraise
			   (push all-upcase temp)
			   (push (intern-str nameroot) temp)))))
	     (go l))
	    (t (vector-push-extend (schar bytestream index) nameroot)
	       (incf index)
	       (go nr)))
   next (incf index)
    (go l)))


(defun set-superscript-face (sym)
;;; sym is of form abcd^1234
  (let* ((name (symbol-name sym))
	 (pos (position #\^ name :from-end t))
	 (root (subseq name 0 pos))
	 (pre-face (get (intern-str root) 'face))
	 (script-list (if pre-face pre-face (list (intern-str root)))))
  (setf (get sym 'face)
    (append script-list
	    (map 'list #'(lambda (x) (intern-str (format nil "SUP~D" x)))
		 (subseq name (1+ pos)))))))

(defun parse-sup-script (str start maxindex)
  (do ((i start (1+ i))
       (num ""))
      ((= i maxindex) (values num maxindex))
    (if (digit-char-p (char str i))
	(setq num (concatenate 'string num (string (char str i))))
      (return-from parse-sup-script (values num i)))))

;;;The new CMULISP doesn't coerce a one-dimension
;;;char array to a string. Hence we should no longer
;;;take it for granted. 

(defun lexscan-sp ()
 (declare (special bytestream index maxindex))
 (flet ((safe-string-upcase (x) (string-upcase (coerce x 'simple-string))))
  (prog (nameroot temp type)
   l (cond ((or (>= index maxindex) (char= (schar bytestream index) #\)))
	    (return (reverse temp)))
	   ((char-member (schar bytestream index) spacelist) (go next))
	   ((char= (schar bytestream index) #\`)
	    ;; A Backquote introduces a token which is not a wff or type,
	    ;; but may be an arbitrary LISP expression.
	    (push (lexscan-quoted-token) temp)
	    (go l))
	   ((char= (schar bytestream index) #\()
	    (incf index)
	    (setq type
		  (%catch% (parse-type)
			   (fail (throwfail "Type Scan Error - Probably () in type."))))
	    (setq temp (cons (if (atom type) (list type) type) temp))
	    (go l))
	   ((char-member (schar bytestream index) stoplist)
	    (push (if (char= (schar bytestream index) '#\~) 'not
		      (if (char= (schar bytestream index) '#\.) 'dot
			  (intern-char (schar bytestream index))))
		  temp)
	    (go next)))
    ;; the beginning of a nameroot
    (setq nameroot (empty-fill-string))
   nr (cond ((or (= index maxindex)
		 (char-member (schar bytestream index) stoplist))
	     (let ((all-upcase (intern-str (safe-string-upcase nameroot))))
	       (if (get all-upcase 'fo-single-symbol)
		   (push all-upcase temp)
		   (if first-order-mode-parse
		       ;; Next line is incorrect.
		       (if lowercaseraise
			   (dotimes (i (length all-upcase))
			     (push (intern-char (char (symbol-name all-upcase)
						      i)) temp))
			   (dotimes (i (length nameroot))
			     ;; schar does not work in line below because
			     ;; nameroot is not simple string
			     (push (intern-char (aref nameroot i)) temp)))
		       (if lowercaseraise
			   (push all-upcase temp)
			   (push (intern-str nameroot) temp)))))
	     (go l))
	    ;; find a ^ after a nameroot has begun
	    ((and (char= (schar bytestream index) #\^)
		  (> (length nameroot) 0))
	     (multiple-value-bind (strsup next-good-index)
		 (parse-sup-script bytestream (1+ index) maxindex)
	       (if (> (length strsup) 0)
		   (let ((nameroot-len (length nameroot)))
		     (vector-push-extend #\^ nameroot)
		     (dotimes (i (length strsup))
		       (vector-push-extend (schar strsup i) nameroot))
		     (let* ((all-upcase (safe-string-upcase nameroot))
			    (tmp (if lowercaseraise all-upcase nameroot)))
			 (if (get (intern-str all-upcase) 'fo-single-symbol)
			     (push (intern-str all-upcase) temp)
			   (if first-order-mode-parse
			       (let ()
				 (dotimes (i (1- nameroot-len))
				   (push (intern-char (char (string tmp) i)) 
					 temp))
				 (push (intern-str (subseq tmp
							   (1- nameroot-len)))
				       temp)
				 (set-superscript-face (car temp)))
			     (progn 
			       (push (intern-str tmp) temp)
			       (set-superscript-face (car temp))))))
		     (setq index next-good-index)
		     (go l))
		 (progn
		   (vector-push-extend (schar bytestream index)
				       nameroot)
		   (incf index)
		   (go nr)))))
	    (t (vector-push-extend (schar bytestream index) nameroot)
	       (incf index)
	       (go nr)))
   next (incf index)
    (go l))))



(defun lexscan-quoted-token ()
  (declare (special bytestream index maxindex))
  (let ((meta-label (gentemp)))
    (putprop meta-label t 'meta-label)
    (if (not (char= (schar bytestream (1+ index)) #\())
	(putprop meta-label (peel-token) 'lisp-token)
	(progn
	 (incf index)
	 (let ((old-index index)
	       (first-token (peel-token)))
	   (if (and (symbolp first-token) (get first-token 'wffop))
	       ;; Had something like `(lcontr [[lambda x P x] y]).
	       (progn
		(putprop meta-label (lexscan-sp) 'lexlist)
		(putprop meta-label first-token 'meta-label-wffop)
		;;(incf index)  ; should now be done in peel-token
		)
	       ;; Had something like `(1 2 3), e.g. as occurrence-list.
	       (progn
		(setq index old-index)
		(putprop meta-label (peel-list) 'lisp-token))))))
    meta-label))

(defun peel-token ()
  (declare (special bytestream index maxindex))
  (with-input-from-string (s bytestream :index index :start (+ index 1))
    (read s)))

(defun peel-list () 
  (declare (special bytestream index maxindex))
  (with-input-from-string (s bytestream :index index :start index)
    (read s)))

(defun parse-type ()
  (declare (special bytestream index maxindex))
  (do ((stack nil)
       (char (unless (= index maxindex) (schar bytestream index))
	     (unless (= index maxindex) (schar bytestream index))))
      ((or (= index maxindex) (char= char #\)))
       (incf index)
       (if stack stack (throwfail "Illegal empty typesymbol.")))
    (cond ((char= char #\()
	   (incf index)
	   (setq stack (if stack (cons stack (parse-type)) (parse-type))))
	  ((char= char #\Space)
	   (incf index))
	  (t (let ((typesym (intern-char (char-upcase char))))
	       (setq stack (if stack (cons stack typesym) typesym))
	       (unless (or (get typesym 'typeconst) (get typesym 'typeabbrev))
		       (putprop typesym t 'parsed-type))
	       (incf index))))))

(defgwff-type string-type
  (checkfn stringp)
  (getfn rd-string)
  (mhelp "string : quoted sequence of symbols."))

(defgwff-type string-bound-var
  (checkfn stringbound-p)
  (getfn rd-string-bound)
  (mhelp "bound variable: variable bound to a string."))

(defun stringbound-p (element)
  (and (symbolp element) (boundp element) (stringp (eval element))))

(defun rd-string-bound (element)
  (rd-string (eval element)))

#+tops-20
(eval-when (compile eval)
  (format *error-output* "~%Remember that due to a compiler bug ~
	  you will have to edit the WFFIN-3.LAP file.~%~
	  Please replace one occurrence of ` . ' by ` \\. '."))
