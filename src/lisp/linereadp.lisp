;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :core)
(part-of BARE)

(deffile linereadp
  (part-of BARE)
  (extension lisp)
  (mhelp "Functions for reading the input from the command line."))

;;;
;;; Author: Frank Pfenning (original) & Dan Nesmith (30AUG90)
;;;
;;; This file defines
;;;
;;; (linereadp &optional input-stream)
;;; (linereadpp &optional prompt input-stream output-stream)
;;;
;;; LINEREADP is like the UCI-Lisp LINEREADP.
;;; It does successive READs and returns
;;; the list of elements read, terminating on a #\newline in between
;;; READs, as long as it is between objects.
;;;
;;; 30AUG90: All this stuff was overhauled.  Now we have the ability
;;; to do command history substitution, variable substitution, and
;;; aliases.  Also multiple commands per input line! See descriptions below 
;;; for how this works.  DAN

;;; LINEREADP works as follows.  read-line gets a new line of input
;;; from the user. tokenize-line is called to parse the input line
;;; into tokens, strings representing different lisp objects, e.g.,
;;; symbols, lists, whatever.  We might have been in the middle of an
;;; object when read-line terminated (e.g., a string containing a
;;; newline) so tokenize-line tells that.  If we were in the middle,
;;; read-line is called to get a new input line, then tokenize-line is
;;; called again from the beginning of the object. 
;;; Once the input line(s) is/are tokenized, we put this input in the
;;; command history list (if keep-history is non-nil).  Next we apply
;;; any alias substitutions, then variable (flag) substitutions,
;;; then any command-completions.  Remember that so far, our tokens
;;; are all strings.  This makes them easy to use.  Finally, lisp-ize
;;; just reads each string to get an object, and we return a list of
;;; objects, symbols, lists, etc. 

(defun linereadp (&optional (input-stream *standard-input*)
			    (keep-history nil)
			    (do-history-subs nil)
			    (do-alias-subs t)     ; was nil
			    (do-flag-subs nil)
			    (do-command-completion t)) ; was nil
  (declare (special do-history-subs do-alias-subs do-flag-subs
		    do-command-completion)) 
  (let ((eof-value (string control-d-char)))
    (do ((instring (read-line-sym input-stream nil eof-value) ; ceb 2/21/02
		   (unless finished-p
		     (concatenate 'string unfinished-part
				  (string #\newline)
				  (read-line-sym input-stream nil ; ceb 2/21/02
						 eof-value))))
	 (new-tokens nil)
	 (tokens nil (nconc tokens new-tokens))
	 (unfinished-part "")
	 (finished-p nil))
	(finished-p  
	    (when tokens 
	      (lisp-ize 
	       (command-completion 
		(do-flag-substitutions 
		 (do-alias-substitutions 
		  (update-history keep-history
				  (do-command-history-substitutions tokens t))))))))
      (when (and restore-file-p 
		 (eq eof-value instring))
	(throw 'restore-file-end nil))
      (multiple-value-setq (new-tokens unfinished-part finished-p)
	(tokenize-line instring)))))

; cebrown 2/21/02 - read-line-sym just calls read-line if it gets a stream.
; if it gets a symbol, it waits for the symbol to get the property 'response to be set -
; this is used when TPS is being used through a user interface and the interface is
; prompting for input.
#+(and allegro-version>= (version>= 5 0))
(defun read-line-sym (input-stream eof eof-value)
  (if (streamp input-stream)
      (read-line input-stream eof eof-value)
    (progn
      (mp:process-wait (mp:process-name sys:*current-process*)
		       #'(lambda () (get input-stream 'response)))
      (let ((r (get input-stream 'response)))
	(princ r)
	(setf (get input-stream 'response) nil)
	(terpri)
	r))))

#-(and allegro-version>= (version>= 5 0))
(defun read-line-sym (input-stream eof eof-value)
  (if (streamp input-stream)
      (read-line input-stream eof eof-value)
    (if (symbolp input-stream)
	(prog1
	    (get input-stream 'response)
	  (terpri))
      "")))

;;; just read the list of tokens and make lisp objects from them

; cebrown changed this to deal with the case where the token is
; a string of dots (this used to cause an error)
(defun lisp-ize (tokens)
  (mapcar #'(lambda (x) (if (all-dots x) x (with-input-from-string (in x) (read in))))
          tokens))

(defun all-dots (x)
  (and (> (length x) 0)
       (dotimes (i (length x) T)
		(unless (eq (aref x i) #\.)
		  (return nil)))))

;;; Print the prompt, then call linereadp.

(defun linereadpp
  (&optional (prompt ">")
	     (keep-history nil)
	     (do-history-subs nil)
	     (do-alias-subs t) ;was nil
	     (do-flag-subs nil)
	     (do-command-completion t) ;was nil
	     (input-stream *standard-input*)
	     (output-stream *standard-output*))
  (when (eq style 'istyle)
    (send-top-level))
  (princ prompt output-stream)
  (force-output)
  (linereadp input-stream keep-history do-history-subs do-alias-subs
	     do-flag-subs do-command-completion))


;;; ******************* COMMAND COMPLETION ******************

;;; Check the first token.  If it ends in an escape character, then
;;; try to find completions for it.  If none or more than one, do a
;;; throwfail.  Otherwise put the unique completion in place of the
;;; first token and return all tokens.

(defflag completion-options
  (flagtype boolean)
  (default t)
  (subjects maintain)
  (mhelp "If T, then the user will be offered a choice between
multiple completions of a command. Also, the commands offered 
will come from the current top level, the main top level and the 
flags.
If NIL, command completion will try first the current top level,
then the main top level, and then the flags, and will fail if the 
first of these which contains any completions also contains
multiple completions.
For example (when T)
<1>displ& 

3 matching commands or flags have been found.
1) DISPLAYFILE
2) DISPLAY-TIME
3) DISPLAYWFF
4) None of these.
Input a number between 1 and 4: [1]>

(when NIL)
<2>displ& 
TPS error while reading.
Multiple completions for DISPL: DISPLAYFILE DISPLAY-TIME 
"))

(defun command-completion (tokens)
  (declare (special do-command-completion))
  (if (and tokens do-command-completion)
      (let* ((command-name (car tokens))
	     (last-char (char command-name (1- (length command-name)))))
	(if (or (eql last-char escape-char) (eql last-char #\@))
	    (let* ((sym (intern (string-upcase 
				 (subseq command-name 0 (1- (length
							     command-name))))
				(find-package "CL-USER")))
		   (completions (find-completions-real sym)))
	      (cond ((null completions) 
		     (throwfail (format nil "No completions for ~A.~%" sym)))
		    ((cdr completions)
		     (if completion-options
			 (let ((count 0))
			   (msgf (length completions) " matching commands or flags have been found." t)
			   (dolist (elt completions)
				   (incf count)
				   (msgf count ") " elt t))
			   (incf count)
			   (msgf count ") None of these." t)
			   (setq completions (list (nth (1- (get-comp-option count)) completions)))
			   (if (null (car completions))
			       (throwfail "Aborted.")
			     (cons (string (car completions)) (cdr tokens))))
		       (throwfail (format nil "Multiple completions for ~A: ~{~A ~}~%"
					  sym
					  completions))))
		    (t
		     (cons (string (car completions)) 
			   (cdr tokens)))))
	  tokens))
  tokens))
    
(defun get-comp-option (count)
  (declare (special *using-interface*))
  (do ((input nil))
      ((and (funcall (get 'posinteger 'testfn) input) (< input (1+ count))) input)
      (prompt-read input nil 
		   (msgf "Input a number between 1 and " count ":")
		   'posinteger 1 ((? (msgf "A number on the above list")) (?? (mhelp 'posinteger))))))

;;; See if there is a member of the global list whose prefix matches prefix.

(defun find-matching-prefix (prefix global-list)
  (let* ((prefix-str (string prefix))
	 (length (length prefix-str)))
    ;; For now we check for each symbol whether it's accessible right
    ;; here rather than preparing the global list at each use-package.
    (do ((glist (cdr global-list) (cdr glist))
	 (elt (car global-list) (car glist))
	 (mlist nil (if (or (consp elt)
			    (< (length (string elt)) length)
			    (not (string-equal prefix-str (string elt)
					       :end2 length))
			    (not (accessible-p elt)))
			mlist
			(cons elt mlist))))
	((null elt) mlist))))

;;; Figure out which global list to use, then call find-matching-prefix.

(defun find-completions-real (symbol)
  (let ((compl (find-completions symbol)))
    (if completion-options 
	(progn 
	  (setq compl (remove-duplicates (append compl (find-flag-completions symbol))))
	  compl)
      (if (null compl) (find-flag-completions symbol) compl))))

(defun find-completions (symbol)
  (let ((list (find-matching-prefix
	       symbol
	       (eval (get (get top-level 'top-level-category) 'global-list)))))
    ;if we can't complete it at this top level, try the main top level.
    (if (and completion-options (not (eq top-level 'cmd-top)))
	(setq list (append list (find-matching-prefix symbol global-mexprlist)))
      (when (and (null list) (not (eq top-level 'cmd-top)))
	    (setq list (find-matching-prefix symbol global-mexprlist))))
    list))

(defun find-flag-completions (symbol)
  (find-matching-prefix symbol global-flaglist))

;;; *************** FLAG (VARIABLE) SUBSTITUTIONS

;;; For each token, if it begins with a $, and the remainder is a
;;; bound symbol, e.g. $expert-list, then replace that token by a
;;; printed representation of the variable's value.  Available only to
;;; expert users.

(defun do-flag-substitutions (tokens)
  (declare (special do-flag-subs))
  (if (and do-flag-subs expertflag tokens)
      (mapcar #'(lambda (x)
		  (if (and (eql #\$ (char x 0))
			   (> (length x) 1)
			   (boundp (intern (string-upcase (subseq x 1)))))
		      (prin1-to-string (symbol-value (intern
						      (string-upcase
						       (subseq x 1)))))
		    x))
	      tokens)
    tokens))


;;; **************** THE COMMAND HISTORY LIST ***********


;;; *COMMAND-HISTORY* is a list that holds, in reverse order, the last
;;; HISTORY-SIZE number of input command lines.  Each element of
;;; *COMMAND-HISTORY* is a pair.  The first of the pair is the input
;;; line number (the value of *command-history-counter* when the line
;;; was read in).  The rest of the pair is the tokenized line entered
;;; at that time.

(defvar *command-history* (make-list 25 :initial-element nil))

;;; Analogous to csh history shell variable.

(defflag history-size
  (flagtype null-or-posinteger)
  (change-fn change-history)
  (subjects otl-vars maintain)
  (default 25)
  (mhelp "Maximum number of commands to save. If NIL, all commands
will be saved."))

;;; Properly downsizes or enlarges *command-history* as user requests.

(defun change-history (flag-name new-value old-value)
  (declare (ignore flag-name))
  (when (null old-value) (setq old-value (length *command-history*)))
  (cond ((null new-value))
	((< new-value old-value)
	 (setq *command-history* (subseq *command-history* 0 new-value)))
	((> new-value old-value)
	 (setq *command-history* 
	   (nconc *command-history* (make-list (- new-value old-value)
					       :initial-element
					       nil))))))
;;; This is just like the csh history command.

(context subtoplevels)

(defmexpr history
  (argtypes null-or-posinteger yesno)
  (argnames n reverse)
  (mainfns print-history)
  (defaultfns (lambda (n reverse)
		(list (if (eq n '$) history-size n)
		      (if (eq reverse '$) nil reverse))))
  (mhelp "Show history list.  Shows the N most recent events; N defaults to 
the value of HISTORY-SIZE, showing entire history list. Values of N that 
are greater than HISTORY-SIZE have the same effect as the default value. 
REVERSE defaults to NO; if YES, most recent commands will be shown first."))


;;; ******************* HISTORY SUBSTITUTION MECHANISM ****************

;;; Given tokens, applies history substitution to each of them, and
;;; returns the transformed tokens.  Sometimes we want to print the
;;; new tokens (as is done by the csh).  Other times, notably when we
;;; are doing alias substitution, we don't want to print them.
	
(defvar *previous-matched-line* nil)


(defun do-command-history-substitutions (tokens &optional print-changes)
  (declare (special do-history-subs))
  (if do-history-subs
      (progn (setq *previous-matched-line* nil)
	     (let ((new-tokens (mapcan #'history-substitute-top tokens)))
	       (when (and print-changes
			  (not (equal tokens new-tokens)))
		 (format t "~{~A ~}~%" new-tokens))
	       new-tokens))
    tokens))

;;; Print the N most recent history events as desired.

(defun print-history (n reverse)
  (let ((commands-to-print 
	     (subseq *command-history*
		 0 n)))
  (dolist (hist (if reverse 
		    commands-to-print
		  (reverse commands-to-print))
	    (terpri))
    (when (car hist)
      (format t "~&~6D  ~{~A ~}" (car hist) 
	      (mapcar #'(lambda (str) (string-right-trim (list
							  escape-char)
							 str))
		      (cdr hist)))))))

;;; KEEP-HISTORY will be NIL when we are using linereadp to prompt for
;;; arguments to functions.  This just adds the new line to the front
;;; and, if history-size is finite, kicks out the last line.
;;; Then the *command-history* counter is incremented, and the new
;;; line is returned.

(defun update-history (keep-history new-line)
  (when (and keep-history new-line)
    (setq *command-history*
      (cons (cons *command-history-counter* new-line)
	    (if history-size
		(subseq *command-history* 0 
			(1- history-size))
	      *command-history*)))
    (incf *command-history-counter*))
  new-line)

;;; This function allows us to handle embedded history substitutions.
;;; When it finds a substring that starts with a !, it calls
;;; history-substitute with that substring.

(defun history-substitute-top (token)
  (let ((pos t)
	front subs 
	(result (list token)))
    (flet ((position-of-unescaped-! (token)
	     (do ((index 0)
		  (len (length token)))
		 ((or (>= index len)
		      (eql (char token index) #\!))
		  (if (= index len) 
		      nil
		    index))
	       (if (eql (char token index) #\\)
		   (incf index 2)
		 (incf index 1)))))
      (do ()
	  ((or (null pos) (string= "!" token))
	   (delete "" result :test #'string=))
	(setq token (car (last result)))
	(setq pos (position-of-unescaped-! token))
	(when pos
	  (setq front (subseq token 0 pos))
	  (setq subs (history-substitute (subseq token pos)))
	  (setq result 
	    (nconc (butlast result)
		   #+comment(cons (concatenate 'string front (car subs))
				  (cdr subs))
		   ;; if history-sub is made inside a string, for example,
		   ;; we don't want to break up that string. Following
		   ;; is better 
		   (tokenize-line 
		    (format nil "~A~A~{ ~A~}" front (car subs) (cdr subs)))
		   )))))))



(defun history-substitute (token)
  (let* ((len (length token))
         (ch (unless (zerop len) (char token 0))))
    (declare (special len))
    (if (or (not (eql ch #\!))
            (= len 1))
        (list token)
      (progn
        (multiple-value-bind (line-num range pos)
            (parse-history-subst token)
	  (setq *previous-matched-line* line-num)
          (let ((old-line (get-history-line line-num)))
            (get-range-of-line old-line range token pos)))))))

;;; Range is in the form (first-word last-word) and is inclusive. 
;;; We get that range of words from old-line (if range is valid), then
;;; stick on the remainder of token to the last of those words.

(defun get-range-of-line (old-line range token pos)
  (let ((len (length old-line))
        (new-line nil)
	(remainder (subseq token pos)))
    (when (equal '(*) range) 
      (if (= 1 len)
	  (return-from get-range-of-line 
	    (list remainder))
	(setq range '(1 last))))
    (setq range
      (substitute (1- len) 'last
		  (substitute (- len 2) 'last-1 range)))
    ;; matched-word will appear only if no wordnum was returned in
    ;; parse-history-subst 
    (when (memq 'matched-word range)
      (throwfail (format nil "~A: Bad ! arg selector.  % selector can be
used only with the !? designator.~%"
			 token)))
    (when (> (car range) (cadr range))
      (throwfail (format nil "~A: Bad ! arg selector.  Last of range is less than first.~%"
                 token)))
    (when (< (car range) 0)
      (throwfail (format nil 
			 "~A: Bad ! arg selector.  First of range is negative.~%"
                 token)))
    (when (>= (cadr range) len)
      (throwfail (format nil "~A: Bad ! arg selector. Last of range is too high.~%" token)))
    (setq new-line (subseq old-line (car range) (1+ (cadr range))))
    (substitute (concatenate 'string (car (last new-line))
                             remainder)
                (car (last new-line))
                new-line
                :from-end t
                :count 1)))

;;; We use the same rules for command history substitutions as the
;;; unix csh.  
;;; EXCEPT: 1. we allow only event designator and word specifier, not any
;;;         modifiers after that.
;;;         2. the substitution mechanism that comes into play when
;;;         '^' is the first letter of a line is not implemented.




;;; Example: suppose the history is
;;; 3 diy x2107 
;;; and the token is "!3:1-$a" then this function should return
;;; the values 3, (1  1) and 6.

(defun parse-history-subst (token)
  "Return 3 values: the line number to which this token refers, the
range of words selected, and the first position of the token which is
not part of the history-substitution."
  (declare (special len))
  (let ((line-num nil)
        (range nil))
    (do* ((index 1)
          (ch nil)
          (wordnum nil))
        ((or (= index len)
             (and line-num range)
             (and (= index len) line-num))
         (values line-num 
                 (substitute (or wordnum 'matched-word) 
			     'matched-word 
                             (or range (list 0 'last))) index))
      (setq ch (char token index))
      (if line-num
	  ;; now parsing range
          (cond ((member ch '(#\^ #\$ #\* #\- #\%))
                 (multiple-value-setq (range index)
                   (parse-range token index)))
                ((eql ch #\:)
                 (multiple-value-setq (range index)
                   (parse-range token (1+ index))))
                (t (setq range '(0 last))))
        (cond ((eql ch #\!)
               (setq line-num -1)
               (setq index (1+ index)))
              ((digit-char-p ch)
               (multiple-value-setq (line-num index)
                 (get-int-from-string token index)))
              ((eql ch #\-)
               (multiple-value-setq (line-num index)
                 (get-int-from-string token (1+ index)))
               (setq line-num (- line-num)))
              ((eql ch #\?)
               (let* ((end (position #\? token :start (1+ index)))
                      (seekstring (subseq token (1+ index) 
                                          (or end len))))
                 (multiple-value-setq (line-num wordnum)
                   (find-line-num-with-string-anywhere seekstring))
                 (unless line-num
                   (throwfail (format nil "~A: Event not found.~%" seekstring)))
                 (setq index 
                   (if end (1+ end)
                     len))))
              ((member ch '(#\^ #\$ #\% #\- #\* #\:))
               (setq line-num (or *previous-matched-line* -1)))
	      ;; must refer to a line that begins with following string
              (t	
               (let* ((end (position #\: token :start (1+ index)))
		      (seekstring
		       (subseq token index (or end len))))
                 (setq line-num
                   (find-line-num-with-string-begin seekstring))
                 (unless line-num
                   (throwfail (format nil "~A: Event not found.~%" seekstring)))
                 (setq index 
                   (if end
                       end
                     len)))))))))

(defun parse-range (str begin)
  (declare (special len))
  (let ((first-num nil)
        (last-num nil)
        (index nil)
        (index2 nil))
    (multiple-value-setq (first-num index)
      (parse-first-range-num str begin))
    (cond ((null first-num)
	   (return-from parse-range (values '(0 last) begin)))
	  ((eq first-num '*)
	   (return-from parse-range (values '(*) index))))
    (multiple-value-setq (last-num index2)
      (parse-second-range-num str index))        
    (unless last-num 
      (return-from parse-range (values (list first-num first-num)
				       index)))
    (values (list first-num last-num) index2)))



(defun parse-first-range-num (str begin)
  (declare (special len))
  (let ((ch (if (< begin len) (char str begin))))
    (cond ((null ch) nil)
          ((eql ch #\^)
           (values 1 (1+ begin)))
          ((eql ch #\$)
           (values 'last (1+ begin)))
          ((eql ch #\%)
           (values 'matched-word (1+ begin)))
          ((eql ch #\-)
           (values 0 begin))
          ((eql ch #\*)
           (values '* (1+ begin)))
          ((digit-char-p ch)
           (get-int-from-string str begin))
          (t nil))))

(defun parse-second-range-num (str begin)
  (declare (special len))
  (let ((ch (if (< begin len) (char str begin)))
        (ch2 (if (< (1+ begin) len) (char str (1+ begin)))))
    (cond ((null ch) nil)
          ((eql ch #\*)
           (values 'last (1+ begin)))
          ((not (eql ch #\-))
           (values nil begin))
          ((null ch2)
           (values 'last-1 len))
          ((eql ch2 #\^)
           (values 1 (+ 2 begin)))
          ((eql ch2 #\$)
           (values 'last (+ 2 begin)))
          ((eql ch2 #\%)
           (values 'matched-word (+ 2 begin)))
          ((digit-char-p ch2)
           (get-int-from-string str (1+ begin)))
          (t (values 'last-1 (1+ begin))))))

(defun get-int-from-string (str begin)
  (parse-integer str :start begin :junk-allowed t))


(defun find-line-num-with-string-anywhere (str)
  (let ((line (find-if #'(lambda (line)
                    (position-if #'(lambda (x)
                                     (search str x))
                                 (cdr line)))
		*command-history*)))
    (if line
	(values (car line) (position-if #'(lambda (x)
					    (search str x))
					(cdr line))))))


(defun find-line-num-with-string-begin (str)
  (car (find-if #'(lambda (line)
		    (let ((tmp (search str (cadr line))))
		      (and tmp (zerop tmp))))
                *command-history*)))

(defun get-history-line (num)
  (let ((line nil))
    (if (< num 0)
        (setq line (cdr (nth (1- (- num)) *command-history*)))
      (setq line (cdr (find num *command-history* :key #'car))))
    (unless line
      (throwfail (format nil "!~A: Event not found.~%" num)))
    line))

;;; ******************* END HISTORY SUBSTITUTION MECHANISM ****************

;;;**************** THE ALIAS MECHANISMS *******************

(context otl-entering)

(defmexpr alias
  (argtypes symbol string)
  (argnames name def)
  (arghelp "Name of symbol to alias (*ALL* to print aliases)" 
	   "Definition (\"\" to see current defn.)")
  (defaultfns
   (lambda (name def)
     (cond ((eq name '$) (list '*all* ""))
	   ((eq name '*all*) (list '*all* ""))
	   ((eq def '$) (list name ""))
	   (t (list name def)))))
  (mainfns alias-command)
  (mhelp "Define an alias DEF for the symbol NAME.  Works just like the
alias command in the Unix csh.  If the value of NAME is *ALL*, all
aliases will be printed; if the value of DEF is the empty string, then
the current alias definition of NAME will be printed.  See UNALIAS."))

(defmexpr unalias
  (argtypes symbol)
  (argnames name)
  (defaultfns)
  (mainfns remove-alias)
  (mhelp "Remove an alias for the symbol NAME.  Like the Unix csh
unalias, except that NAME must exactly match the existing alias; no
filename completion is done."))

(context subtoplevels)

(defun alias-command (symbol string)
  (cond ((eq symbol '*all*)
	 (print-alias))
	((memq symbol '(alias unalias))
	 (throwfail (format nil "~A: Too dangerous to alias that.~%" symbol)))
	((string= string "")
	 (print-alias symbol))
	(t 
	 (make-alias symbol string))))

;;; Aliases are stored in the hash-table *alias-hash-table*.  Each alias
;;; is represented by an alias-elt structure.  The *alias-hash-table* is
;;; indexed by interning symbols in the keyword package (where they 
;;; shouldn't bother anybody). 

(defvar *alias-hash-table* (make-hash-table :test #'eq))
(defconstnt *alias-pack* (or (find-package "LINEREADP-ALIAS")
			      (progn (make-package "LINEREADP-ALIAS")
				     (find-package "LINEREADP-ALIAS"))))

;;; The name slot of an alias-elt structure should be a symbol.  The
;;; def should be a list of strings (like a tokenized input line).

(defstruct alias-elt 
  name
  def)

;;; Want to make string STR an alias for symbol SYM.  First tokenize
;;; STR to get a list of strings.  If the user gave a string that had
;;; no tokens, then just print the existing alias for sym (if any).
;;; This differs from csh alias, which lets you define aliases that
;;; expand out to nothing.

(defun make-alias (sym str)
  (let* ((def (tokenize-line str))
	 ;; user may have passed a string that is just whitespace
	 (new-alias (if def (make-alias-elt :name 
					    (intern (string-downcase
						     (symbol-name sym))
						    *alias-pack*)
				   :def def))))
    (if new-alias
	(progn
	(setf (gethash (alias-elt-name new-alias)
		       *alias-hash-table*)
	  new-alias)
	(eval (nconc (list 'definfo sym 
			   (list 'mhelp 
				 (concatenate 'string 
					      (princ-to-string sym)
					      " is an alias, defined as:
" str))))))
      (print-alias sym)))
  (values))
    

;;; Remove the alias-elt whose name slot is SYM from the *alias-hash-table*.

(defun remove-alias (sym)
  (let ((name (intern (string-downcase (symbol-name sym)) 
		      *alias-pack*)))
    (remhash name *alias-hash-table*)
    (values)))

;;; Find the alias that corresponds to string STR.  Return its def slot.

(defun find-alias (str)
  (let* ((name (intern (string-downcase str) *alias-pack*))
	 (alias-elt (gethash name *alias-hash-table*)))
    (if alias-elt
	(alias-elt-def alias-elt)
      (progn (unintern name *alias-pack*) nil))))

;;; Print alias for a particular SYMBOL or print all aliases.  Format
;;; is similar to what is used in Unix.

(defun print-alias (&optional (symbol nil arg-p))
  (if arg-p
      (let ((alias-def (find-alias symbol)))
	(when alias-def
	    (format t "~&~A~9T ~{~A ~}" (symbol-name symbol)
		    alias-def)))
    (let* ((aliases nil))
      (maphash #'(lambda (key val) (declare (ignore key))
			 (push val aliases)) 
	       *alias-hash-table*)
      (dolist (alias-elt (sort aliases #'string-lessp 
			       :key #'alias-elt-name))
	(format t "~&~A~9T ~{~A ~}" 
		(symbol-name (alias-elt-name alias-elt))
		(alias-elt-def alias-elt))))))

;;; Following are for use in .ini files, etc.

(defmacro alias (symbol string)
  (cond ((and (symbolp symbol) (stringp string))
	 `(make-alias ',symbol ',string))
	((not (symbolp symbol))
	 `(complain (format nil "Error in alias: ~S is not a symbol."
			    ',symbol)))
	((not (stringp string))
	 `(complain (format nil "Error in alias: ~S is not a string."
			    ',string)))))


(defmacro unalias (symbol)
  (if (symbolp symbol)
      `(remove-alias ',symbol)
    `(complain (format nil "Error in unalias: ~S is not a symbol."
		       ',symbol))))


;;; ************* ALIAS LOOKUP **************************

;;; This var is used to keep track of which aliases have already been
;;; expanded, preventing infinite loops. 

(defvar *already-expanded* nil)

;;; Find alias for the first string in TOKENS.  If there is one,
;;; do command substitution on it, temporarily making the TOKENS 
;;; the last "command" entered.   
;;; Return NIL if no alias is found.  This tells us to stop doing alias
;;; substitution.
;;; initial-tokens is what the user originally typed in, while tokens
;;; may be a sublist of that, or a de-aliased sublist.

;;; By putting a & between commands, user can enter multiple commands
;;; on one line.  So we have to do alias substitutions on each command.
;;; following allows aliases to themselves contain aliases.
;;; Remember that commands are separated by a "&".
                                                                
;;; Do alias substitutions, and repeat until we can't do any more.
;;; initial-tokens is what the user originally typed in, while tokens
;;; may be a sublist of that, or a de-aliased sublist.

;;; 1. split input tokens into separate commands
;;; 2. apply do-alias-substitutions1 to each command, return list of commands
;;; to replace them
;;; 3. Put & between each command before returning the commands all
;;; appended together.

(defun do-alias-substitutions (tokens &optional (*already-expanded* nil))
  (declare (special do-alias-subs))
  (labels ((get-subcommand (list n)
	       (let ((start-pos (nth-& list n))
		     (stop-pos (nth-& list (1+ n))))
		 (if start-pos
		     (or (subseq list (1+ start-pos) stop-pos) 'quit)
		   'quit)))
	   (nth-& (list n)
	       (if (zerop n)
		   -1
		 (let ((n-1th-& (nth-& list (1- n))))
		   (if n-1th-& 
		       (position "&" list :start (1+ n-1th-&)
				 :test #'string=))))))
    (let ((list-of-subcommands
	   (do* ((n 0 (1+ n))
	       (subcommand-n (get-subcommand tokens n)
			     (get-subcommand tokens n))
	       (list (if (eq subcommand-n 'quit) 
			 nil
		       (list subcommand-n))
		     (if (eq subcommand-n 'quit) 
			 list
		       (cons subcommand-n list))))
	     ((eq subcommand-n 'quit) (nreverse list)))))
      (if do-alias-subs
	  (butlast
	   (apply #'append
		  (mapcar #'(lambda (l) 
			      (let ((new-comm (do-alias-substitutions1 l)))
				(if (string= "&" (car (last new-comm)))
				    new-comm
				  (append new-comm '("&")))))
			  list-of-subcommands)))
       tokens))))

;;; Call do-alias-substitutions-aux on tokens.  If it makes an alias
;;; substitution (returns non-nil), then recursively call
;;; do-alias-substitutions, but now binding *already-expanded* with
;;; the value returned by do-alias-substitutions-aux.
;;; Otherwise, just return input tokens.

(defun do-alias-substitutions1 (tokens)
;;; we know that tokens is a single command.  
  (setq tokens (command-completion tokens))
  (multiple-value-bind (new-tokens *already-expanded*)
      (do-alias-substitutions-aux 
       tokens)
    (if new-tokens
	(do-alias-substitutions new-tokens *already-expanded*)
      tokens)))

;;; Find alias for the first string in TOKENS.  If there is one,
;;; do command substitution on it, temporarily making the TOKENS 
;;; the last "command" entered.   
;;; Return two values.  The first is either the alias/command substitution 
;;; for TOKENS, or NIL if TOKENS doesn't begin with an alias.
;;; Second value is value of *already-expanded*, either its original
;;; value or adding the alias which has been expanded.

(defun do-alias-substitutions-aux (tokens)
  (let* ((do-history-subs t)
	 (command-name (car tokens))
         (alias-found 
          (unless (member command-name *already-expanded* :test #'string-equal)
            (find-alias command-name))))
    (declare (special do-history-subs))
    (if alias-found
      (let* ((*command-history*
	      (cons (cons -1 tokens)
		    *command-history*))
	     (ch-tokens
	      (do-command-history-substitutions alias-found)))
	(values (if (equal ch-tokens alias-found)
		    (append alias-found (cdr tokens))
		  ch-tokens)
		(cons command-name *already-expanded*)))
      (values nil *already-expanded*))))


;;;**************** END ALIAS MECHANISMS *******************
;;;**************** BEGIN READING LISP TOKENS *************

;;; The idea here is to follow the rules for reading that are in CLtL.
;;; We are given the input line, and we split up the line into
;;; *tokens*, each one a string.  These tokens may themselves be, say,
;;; symbols, arrays, lists, etc.  If the input line ends in the middle
;;; of a token, we get a new input line from the user.  Finally we
;;; return all the input tokens in a list.
;;; Claim: this should handle all standard CL syntax.


(defun tokenize-line (str)
  (do ((next-start 0)
       (last-start 0)
       (len (length str))
       (new-token t)
       (tokens nil (if (and new-token (not (zerop (length new-token))))
                       (cons new-token tokens) 
                     tokens)))
      ((or (null new-token) 
           (= len next-start))
       (values (nreverse tokens) 
               (if (null new-token) (subseq str last-start))
               (if new-token t nil)))
    (declare (special len))
    (setq last-start next-start)
    (multiple-value-setq (new-token next-start)
      (get-next-token str next-start))))

(defun get-next-token (str begin)
  "Input string STR and index BEGIN. 
Output: If we can get a full token from STR beginning at BEGIN, returns the 
token (a string) and the index of STR at which the token ends.  Otherwise
returns NIL and BEGIN."
  (declare (special len))
  ;; assume str is not empty string and first elt is not whitespace
  (let ((char (char str begin)))
    (cond ((eql char #\")	
           (read-a-string-token str begin))
          ((or (eql char #\')	
               (eql char #\`)
               (eql char #\,))
           (multiple-value-bind (token end-index)
               (get-next-token str (1+ begin))
             (if token (values (concatenate 'string (string char)
                                            token)
                               end-index))))
          ((eql char #\()	
           (read-a-list-token str begin))
          ((eql char #\))	
	   (msg "; Ignoring unmatched right paren." t)
           (values "" (next-non-whitespace-char str (1+ begin))))
          ((eql char #\#)
           (read-a-dispatch-macro-token str begin))
          ((eql char #\\)	
           (read-a-single-escape-token str begin))
          ((eql char #\|)	
           (accumulate-token-even-multiple str begin ""))
          ((eql char #\;) 
           (values "" (length str)))
          (t
	   ;; hope it's a constituent character! 
           (accumulate-token-even-multiple str begin "")))))

(defun read-a-dispatch-macro-token (str begin)
  "know that begin'th char of str is a #"
  (declare (special len))
  (let ((ch (if (> len (1+ begin)) (char str (1+ begin)))))
    (cond ((null ch) nil)
          ((whitespace-char-p ch)
           (values (subseq str begin (+ 2 begin)) 
                   (next-non-whitespace-char str (+ 2 begin))))
          ((eql ch #\\)
	   ;; this should be a char object
           (multiple-value-bind (token end-index)
               (read-a-single-escape-token str (1+ begin))
             (if token (values (concatenate 'string "#"
                                            token)
                               end-index))))
          ((digit-char-p ch)
           (let* ((end-of-num
                   (position-if-not  #'digit-char-p
                                     str :start (+ 1 begin)))
                  (next-ch (if (> len end-of-num) (char str end-of-num))))
             (cond ((or (null next-ch)
                        (whitespace-char-p next-ch))
		    ;; just send back the # and the number
                    (values (subseq str begin end-of-num)
                            (next-non-whitespace-char str
                                                      end-of-num)))
                   ((member next-ch '(#\( #\*))
		    ;; vectors
                    (multiple-value-bind (token end-index)
                        (get-next-token str end-of-num)
                      (if token (values (concatenate 'string 
                                          (subseq str begin end-of-num)
					  token)
                                        end-index))))
                   ((eql next-ch #\#)
                    (values (subseq str begin (1+ end-of-num))
                            (next-non-whitespace-char str
                                                      (1+ end-of-num))))
                   (t
		    ;; includes rational, array or ref
                    (multiple-value-bind (token end-index)
                        (get-next-token str (next-non-whitespace-char
                                             str
                                             (1+ end-of-num)))
                      (if token
                          (values (concatenate 'string
                                    (subseq str begin (1+ end-of-num))
                                    token)
                                  end-index)))))))
          ((eql  ch #\()
	   ;; vectors without numeric arg
           (multiple-value-bind (token end-index)
               (get-next-token str (1+ begin))
             (if token (values (concatenate 'string "#"
                                            token)
                               end-index))))
          ((eql ch #\|)
           (let ((next-start
                  (read-macro-comment str (+ 2 begin))))
             (if next-start (values "" next-start))))
          ((member ch '(#\* #\' #\, #\: #\. #\B #\b #\O #\o #\X #\x
                        #\S #\s #\+ #\- ))
	   ;; things which have a char then a form
	   ;; e.g., #*, #.,  #'
           (let ((next-spot
                  (next-non-whitespace-char str (+ 2 begin)))
                 (token nil))
             (when (< next-spot len)
               (multiple-value-setq (token next-spot)
                 (get-next-token str next-spot))
               (when token (values (concatenate 'string 
				     (subseq str begin (+ 2 begin))
				     token)
				   next-spot)))))
          (t
           (multiple-value-bind (token end-index)
               (get-next-token str (1+ begin))
             (if token 
		 (values (concatenate 'string "#" token)
			 end-index)
               (values (subseq str begin (+ 2 begin))
                       (next-non-whitespace-char str (+ 2 begin)))))))))

(defun read-a-string-token (str begin)
  (multiple-value-bind (token next-start)
      (read-until-token str (1+ begin) #\")
    (when token
      (values (concatenate 'string "\"" token) next-start))))

(defun read-a-list-token (str begin)
  "Read a list in as a single token."
  (declare (special len))
  (do ((token t)
       (index (1+ begin)))
      ((or (null token)
           (>= index len)
           (eql (char str index) #\)))
       (if (or (null token)
               (>= index len))
           nil
         (values (subseq str begin (1+ index))
                 (next-non-whitespace-char str (1+ index)))))
    (multiple-value-setq (token index)
      (get-next-token str index))))

(defun read-until-token (str begin closech)
  "Return the substring of STR from BEGIN to the next unescaped 
occurrence of CLOSECH.  Also return position of next non-whitespace char after
that.  If no CLOSECH occurs, return NIL."
  (declare (special len))
  (do  ((index begin))
      ((or (>= index len)
           (eql (char str index) closech))
       (if (= index len) 
           nil
         (values (subseq str begin (1+ index))
                 (next-non-whitespace-char str (1+ index)))))
    (if (eql (char str index) #\\)
        (incf index 2)
      (incf index 1))))

(defun read-macro-comment (str begin) 
  "This is to read comments which are delimited by \#\| and \|\#.
Just skips over them, returning no tokens."
  (declare (special len))
  (do  ((index begin)
        (num-found 1))
      ((or (>= (1+ index) len)
           (zerop num-found))
       (if (zerop num-found)
           (next-non-whitespace-char str index)
         nil))
    (cond ((eql (char str index) #\\)
           (incf index 2))
          ((and (eql (char str index) #\|)
                (eql (char str (1+ index)) #\#))
           (decf num-found)
           (incf index 2))
          ((and (eql (char str index) #\#)
                (eql (char str (1+ index)) #\|))
           (incf num-found)
           (incf index 2))
          (t (incf index 1)))))



(defun whitespace-char-p (ch)
  (member ch '(#\tab #\space #\page #\return #\newline #\linefeed)))

(defun next-non-whitespace-char (str index)
  (declare (special len))
  (do ((index index (1+ index)))
      ((or (= index len)
           (not (whitespace-char-p (char str index))))
       index)))



(defun read-a-single-escape-token (str begin)
  "Rule 5 on page 335 of CLtL."
  (when (not (= begin (1- (length str))))
    (accumulate-token-even-multiple str (+ begin 2)
                                    (subseq str begin (+ begin 2)))))


(defun accumulate-token-even-multiple (str begin token-so-far)
  "Rule 8 on page 337 of CLtL."
  (declare (special len))
  (let ((ch (if (< begin len) (char str begin))))
    (cond ((not ch)
           (values token-so-far begin))
          ((eql ch #\\)
           (if (= len (1+ begin))
               nil
             (accumulate-token-even-multiple 
              str (+ begin 2)
              (concatenate 'string 
                token-so-far
                (subseq str begin (+ begin 2))))))
          ((eql ch #\|)
           (accumulate-token-odd-multiple str (1+ begin)
					  (concatenate 'string
					    token-so-far (string
							  ch))))
          ((member ch '(#\' #\( #\) #\" #\; #\`)
                   :test #'eql)
           (values token-so-far begin))
          ((whitespace-char-p ch)
           (values token-so-far (next-non-whitespace-char str begin)))
          (t
	   ;; should be constituent, or non-terminating macro character
           (accumulate-token-even-multiple str (1+ begin)
                                           (concatenate 'string
                                             token-so-far (string
                                                           ch)))))))

(defun accumulate-token-odd-multiple (str begin token-so-far)
  "Rule 9 on page 337 of CLtL."
  (declare (special len))
  (let ((ch (if (< begin len) (char str begin))))
    (cond ((not ch) nil)
          ((eql ch #\|)
           (accumulate-token-even-multiple str (1+ begin)
                                           (concatenate 'string
                                             token-so-far (string
                                                           ch))))
          ((eql ch #\\)
           (if (= len (1+ begin))
               nil
             (accumulate-token-odd-multiple 
              str (+ begin 2)
              (concatenate 'string 
                token-so-far
                (subseq str begin (+ begin 2))))))
          (t 
           (accumulate-token-odd-multiple str (1+ begin)
					  (concatenate 'string
					    token-so-far (string
							  ch)))))))


; moved get-a-number from lib-ops to here so that ETPS can use it.
; - cebrown 8/24/99
(defun get-a-number (count &optional (default 1))
  (declare (special *using-interface*))
  (do ((input nil))
      ((and (funcall (get 'posinteger 'testfn) input) (< input (1+ count))) input)
      (prompt-read input nil 
		   (msgf "Input a number between 1 and " count ":")
		   'posinteger default ((? (msgf "A number on the above list")) (?? (mhelp 'posinteger))))))

(defun get-a-number-list (count &optional (default '(1)))
  (declare (special *using-interface*))
  (do ((input nil))
      ((and (not (null input))
	    (not (memq nil (mapcar #'(lambda (x) (< x (1+ count))) input))))
       input)
      (prompt-read input nil 
		   (msgf "Input a list of numbers between 1 and " count ":")
		   'posintegerlist default ((? (msgf "A list of numbers from the above list")) 
					    (?? (mhelp 'posinteger))))))


