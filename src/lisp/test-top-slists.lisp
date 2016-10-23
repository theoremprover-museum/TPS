;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
;;;
;;; File TEST-TOP-SLISTS
;;;
;;; defines functions to do with searchlists.
;;;

(deffile test-top-slists
  (part-of mating)
  (extension lisp)
  (mhelp "Defines functions to do with searchlists for test-top."))

(context test-searchlists)

(deftest add-function
  (test-argnames name)
  (test-argtypes symbol)
  (test-arghelp "Name of function")
  (mhelp "Add a function to a searchlist. This function will be evaluated on 
every iteration of the search, and will generally reset certain flags.
The special functions defined so far are:
UNIFORM-SEARCH-FUNCTION sets max-utree-depth, max-search-limit and max-substs-quick
using the values of max-search-depth, search-time-limit and max-substs-var respectively,
and then sets TEST-INITIAL-TIME-LIMIT to allow 5 option sets on the first try, then
10, then 15, and so on.
BASIC-SEARCH-THEN-UNIFORM-SEARCH runs the current searchlist once over, allowing
1 hour for each setting of the flags. Then it switches the searchlist to
UNIFORM-SEARCH-2 and continues with that."))

(defun add-function (name)
  (if (fboundp name) (setf (searchlist-function current-sl) name)
    (msgf "There is no such function in TPS." t)))

(deftest show-searchlist
  (test-argnames name)
  (test-argtypes symbol)
  (test-arghelp "Name of searchlist")
  (test-defaultfns (lambda (name) (list (if (eq name '$) current-sl-name name))))
  (mhelp "Show contents of a searchlist."))

(defun show-searchlist (sl)
  (let ((l (find-searchlist sl *global-searchlist*)))
    (if l
	(progn (msgf "Searchlist " (searchlist-name l) " is as follows:")
	       (dolist (flag (searchlist-list l))
		       (msgf flag t))
	       (when (searchlist-function l) (msgf "...plus the function " (searchlist-function l))))
      (msgf "No such searchlist."))))

(defun find-searchlist (name list)
  (if list
      (if (eq (searchlist-name (car list)) name) 
	  (car list)
	(find-searchlist name (cdr list)))
    nil))

(defun known-searchlist-p (name)
  (if (find-searchlist name *global-searchlist*) t nil))

(defun find-searchlist-string (name list)
  (if list
      (if (string= (string-upcase (princ-to-string (searchlist-name (car list)))) name) 
	  (car list)
	(find-searchlist-string name (cdr list)))
    nil))

(defun known-searchlist-string-p (name)
  (if (find-searchlist-string (string-upcase name) *global-searchlist*) t nil))

(deftest new-searchlist
  (test-argnames name)
  (test-argtypes symbol)
  (test-arghelp "Name of new searchlist")
  (test-mainfns new-searchlist)
  (mhelp "Make a new searchlist; i.e. begin a new list of flags to be varied.
This command also changes the current searchlist."))

(defun new-searchlist (name)
  (if (not (known-searchlist-p name))
      (progn (setq current-sl (make-searchlist :list nil :name name))
	     (setq current-sl-name name)
	     (setq *global-searchlist* (cons current-sl *global-searchlist*)))
    (progn (msgf CURRENT-SL-NAME " already exists; making it the current searchlist.")
	   (setq current-sl-name name)
	   (setq current-sl (find-searchlist name *global-searchlist*))))
  nil)
;was:  name)

(deftest quick-define
  (test-argnames name succ)
  (test-argtypes symbol yesno)
  (test-arghelp "Name of searchlist" "Are current flag settings OK?")
  (test-defaultfns (lambda (s u) 
		     (list  (if (eq s '$) (intern "MASTER-SLIST") s)
			    (if (eq u '$) t u))))
  (test-mainfns quick-define)
  (mhelp "Define a searchlist the quick and dirty way!
If the current flag settings are OK (i.e. are a successful mode),
will create a searchlist in which the flags given in the values of 
the TEST-FASTER-* flags (do LIST TEST-TOP for a listing) vary over 
values which ought to give a faster search than the current values.
If the current flag settings are not OK, will create a searchlist in 
which the flags given in the values of the TEST-EASIER-* flags vary
over values which ought to make the search easier than the current 
values.
The maximum number of values for any flag to take is governed 
by TEST-MAX-SEARCH-VALUES."))

(defun quick-define (name succ)
  (if (known-searchlist-string-p (princ-to-string name))
      (if (query (concatenate 'string "The searchlist " (princ-to-string name) " is already defined.
Overwrite it?") t)
	  (progn (setq current-sl (find-searchlist-string (princ-to-string name) *global-searchlist*))
		 (setq current-sl-name (searchlist-name current-sl))
		 (setf (searchlist-list current-sl) nil))
	(throwfail t))
    (new-searchlist name))
  (msg t)
  (if succ
      ;then we mess with TEST-FASTER-*
      (progn
	(dolist (flag test-faster-if-t)
		(unless (eval flag) ;the flag is already T, leave it out.
		    (add-flag flag nil (list t nil))))
	(dolist (flag test-faster-if-nil)
		(if (eval flag) ;the flag is already nil, leave it out.
		    (add-flag flag t (list nil t))))
	(dolist (flag test-faster-if-low)
		(unless (nullor-option-> 1 (eval flag))
			(add-flag flag (eval flag) (lower-value-list flag))))
	(dolist (flag test-faster-if-high)
		(unless (infinite-p (eval  flag))
			(add-flag flag (eval flag) (higher-value-list flag)))))
    ;else we mess with TEST-EASIER-*
      (progn
	(dolist (flag test-easier-if-t)
		(unless (eval flag) ;the flag is already T, leave it out.
		    (add-flag flag nil (list t nil))))
	(dolist (flag test-easier-if-nil)
		(if (eval flag) ;the flag is already nil, leave it out.
		    (add-flag flag t (list nil t))))
	(dolist (flag test-easier-if-low)
		(unless (nullor-option-> 1 (eval flag))
			(add-flag flag (eval flag) (lower-value-list flag))))
	(dolist (flag test-easier-if-high)
		(unless (or (null (eval flag)) (infinite-p (eval flag)))
			(add-flag flag (eval flag) (higher-value-list flag))))))
  (fix-below (list 'max-search-depth 'max-utree-depth) 'min-quick-depth)
  (msgf "Now do SHOW-SEARCHLIST and use REM-FLAG to remove any unwanted flags." t
"Use ADD-FLAG to edit flags whose settings are unsatisfactory." t))

(defun press-down-quick (name)
  (if (known-searchlist-string-p (princ-to-string name))
      (progn (setq current-sl (find-searchlist-string (princ-to-string name) *global-searchlist*))
	     (setq current-sl-name (searchlist-name current-sl))
	     (setf (searchlist-list current-sl) nil))
    (new-searchlist name))
  (dolist (flag test-faster-if-t)
	  (unless (eval flag)
		  (add-flag flag nil (list t nil))))
  (dolist (flag test-faster-if-nil)
	  (if (eval flag)
	      (add-flag flag t (list nil t))))
  (dolist (flag test-faster-if-low)
	  (unless (nullor-option-> 1 (eval flag))
		  (add-flag flag (eval flag) (lower-value-list flag))))
  (dolist (flag test-faster-if-high)
	  (unless (infinite-p (eval  flag))
		  (add-flag flag (eval flag) (higher-value-list flag))))
  (fix-below (list 'max-search-depth 'max-utree-depth) 'min-quick-depth))

(defun push-up-quick (name)
  (if (known-searchlist-string-p (princ-to-string name))
      (progn (setq current-sl (find-searchlist-string (princ-to-string name) *global-searchlist*))
	     (setq current-sl-name (searchlist-name current-sl))
	     (setf (searchlist-list current-sl) nil))
    (new-searchlist name))
  (dolist (flag test-easier-if-t)
	  (unless (eval flag) ;the flag is already T, leave it out.
		  (add-flag flag nil (list t nil))))
  (dolist (flag test-easier-if-nil)
	  (if (eval flag) ;the flag is already nil, leave it out.
	      (add-flag flag t (list nil t))))
  (dolist (flag test-easier-if-low)
	  (unless (nullor-option-> 1 (eval flag))
		  (add-flag flag (eval flag) (lower-value-list flag))))
  (dolist (flag test-easier-if-high)
	  (unless (or (null (eval flag)) (infinite-p (eval flag)))
		  (add-flag flag (eval flag) (higher-value-list flag))))
  (fix-below (list 'max-search-depth 'max-utree-depth) 'min-quick-depth))

(defun fix-below (listofflags flag)
  ;in the current searchlist, fix the values of flag to always lie below the maximum value in listofflags
  ;all flags referred to take integer values
  (let ((maxval 0))
  (dolist (item (searchlist-list current-sl))
	  (if (member (search-item-flag item) listofflags)
	      (dolist (value (search-item-range item))
		      (setq maxval (max value maxval)))))
  (dolist (item (searchlist-list current-sl))
	  (if (eq (search-item-flag item) flag)
	      (setf (search-item-range item) (remove-if #'(lambda (x) (> x maxval)) (search-item-range item))))))) 


(defun nullor-option-> (a b)
  ;just like option-> but treats NIL as INFINITE.
  (if (null b) nil
    (if (null a) t
      (option-> a b))))

(defun lower-value-list (flag)
  ;return a list of at most test-max-search-values equally-spaced 
  ;integers below the current value of the flag
  (let* ((space-available (if (or (null (eval flag)) (infinite-p (eval flag))) 
					;max-search-limit has NIL = infinite!
			      100000 (eval flag)))
	 (can-go-to-zero (memq (get flag 'flagtype) '(integer+ integer integer+-or-infinity)))
	 (line-jump-1 (if can-go-to-zero 
			  (truncate (/ space-available test-max-search-values))
			(truncate (/ space-available (1+ test-max-search-values)))))
	 (line-jump (if (= 0 line-jump-1) 1 line-jump-1)))
    (do* ((line space-available (- line line-jump))
	 (list-so-far (list (eval flag)) (cons line list-so-far)))
	((or (if can-go-to-zero (option-> 1 line) (option->= 1 line)) 
	     (> (length list-so-far) test-max-search-values))
	 (reverse list-so-far)))))

(defun higher-value-list (flag)
  ;return a list of at most test-max-search-values equally-spaced
  ;integers above the current value of the flag
  (let* ((value (eval flag))
	 (line-jump-1 (truncate (/ value 10)))
	 (line-jump (if (= 0 line-jump-1) 1 line-jump-1)))
    ;effectively : steps of 1 for <=19, 2 for <=29,... 
    (do ((line value (+ line line-jump))
	 (list-so-far nil (cons line list-so-far)))
	((> (length list-so-far) test-max-search-values) (reverse list-so-far)))))

(deftest vary-mode
  (test-argnames modename slistname use-mode)
  (test-argtypes tps-mode symbol yesno)
  (test-arghelp "Name of mode to vary" "Name of searchlist to create" "Set current flag values to those in the mode?")
  (test-defaultfns (lambda (m s u) 
		     (list m 
			  ; (if (known-searchlist-string-p 
			  ;	(concatenate 'string (princ-to-string m) "-slist"))
			  ;     s
			     (if (eq s '$) (intern (concatenate 'string (princ-to-string m) "-SLIST"))
			       s);)
			   (if (eq u '$) t u))))
  (test-mainfns vary-mode)
  (mhelp "Go through an existing mode, flag by flag, creating a searchlist
by picking out relevant flags from it. All useless flags (i.e. ones that 
cannot affect the search time) will be automatically stripped out. The
default flag value in the searchlist will be its value in the mode.
You can also optionally set the current flag values to the values 
in the mode (equivalent to the MODE command)."))
  
(defun vary-mode (m s u)
  (if (known-searchlist-string-p (princ-to-string s))
      (if (query (concatenate 'string "The searchlist " (princ-to-string s) " is already defined.
Overwrite it?") t)
	  (progn (setq current-sl (find-searchlist-string (princ-to-string s) *global-searchlist*))
		 (setq current-sl-name (searchlist-name current-sl))
		 (setf (searchlist-list current-sl) nil))
	(throwfail t))
    (new-searchlist s))
  (msg t)
  (if u (mode m))
  (let ((flaglist (remove-if #'(lambda (x) (memq (car x) (append *dangerous-flag-list* *stupid-flag-list*))) (get m 'flag-settings))))
    (dolist (flagpair flaglist)
	    (let ((flag (car flagpair))
		  (value (cadr flagpair))
		  (flagval (eval (car flagpair))))
	      (msg "Flag : " flag t 
		    "Value now : " flagval t
		    "Value in mode : " value t t)
	      (if (query "Add this flag to the searchlist?" nil)
		  (eval (test-command-interpreter (list 'add-flag flag value)))
		(if (and (not u)
			 (not (string= (princ-to-string (gettype (get flag 'flagtype) value)) (princ-to-string flagval)))
			 (query (concatenate 'string "Set current value of flag to " (princ-to-string value) "?") t))
		    (set-flag flag value)))
	      (msg t)))))


(deftest searchlists
  (mhelp "Print a list of all searchlists currently in memory."))

(defun searchlists ()
  (dolist (sl *global-searchlist*) (msg (searchlist-name sl) t)))

(deftest add-flag
  (test-argnames flag init range)
  (test-argtypes tpsflag anything anything-list)
  (test-arghelp "Flag to be added" "Initial value of flag (RETURN for current value)" "List of possible values to use")
  (test-defaultfns (lambda (x y z) (list x
					 (if (eq y '$) "*current*" y)
					 z)))
  (test-mainfns add-flag)
  (mhelp "Add a single flag to the current searchlist.
To change the current searchlist, use NEW-SEARCHLIST."))


(defun add-flag (fl in rn)
  (if (and (stringp in) (string= in "*current*")) (setq in (eval fl)))
  (if (null rn) (setq rn (list nil)))
  (let ((sl current-sl-name))
  (if (and (check-add-flag sl fl in rn) (check-sensible fl in rn))
      (progn 
	(let ((l (find-searchlist sl *global-searchlist*)))
	  (setq rn (adjoin in rn))
	  (let ((new-si (cons (make-search-item :flag fl :current in :default in :range rn)
			      (edit-sl (searchlist-list l) fl))))
	    (setf (searchlist-list l) new-si)
	    t)))
    nil)))

(defun check-add-flag (sl fl in rn)
  (if (not (known-searchlist-p sl)) 
      (progn (msg t "I don't have a searchlist called " sl "." t 
					  "Use SEARCHLISTS to get a list of all current searchlists, or NEW-SEARCHLIST to start a new one.") nil)
    (if (not (tpsflag-p fl)) (progn (msg t "No such flag as " fl "." t) nil)
      (let ((ft (get fl 'flagtype))
	    (ok t))
	(if (not (funcall (get ft 'testfn) in)) (progn (msg t in " is not of type " ft ", and so can't be a value for " fl "." t) nil)
	  (progn (dolist (each rn) (if (not (funcall (get ft 'testfn) each)) (setq ok nil)))
		 (if (not ok) (progn (msg t "Something in " rn " is not of type " ft "." t) nil) 
		   t)))))))

(defun check-sensible (flag init range)
  (if (eq (length (adjoin init range)) 1) 
      (progn (msg t "Range of only one value for " flag " is pointless." t "Not adding the flag to the list." t )
	     nil)
    (if (member flag *stupid-flag-list*)
	(progn (msg t "The setting of " flag " is unlikely to affect the search time." t "Not adding this flag to the searchlist." t)
	       nil)
      (if (member flag *dangerous-flag-list*)
	  (progn (msg "The flag " flag " is used by the test-search; you can't vary it." t "Not adding this flag to the searchlist." t)
	       nil)
    t))))

(deftest rem-flag
  (test-argnames flag)
  (test-argtypes tpsflag)
  (test-arghelp "Flag to be removed")
  (test-mainfns rem-flag)
  (mhelp "Remove a single flag from the current searchlist.
To change the current searchlist, use NEW-SEARCHLIST."))

(defun rem-flag (flag)
  (let ((l current-sl))
    (if l (progn (setf (searchlist-list l) (edit-sl (searchlist-list l) flag)) 
		 (msg "Removed.")
		 t) 
      (msg t "No such searchlist."))))

(defun edit-sl (list flag)
  (if (null list) nil
    (if (eq (search-item-flag (car list)) flag)
	(edit-sl (cdr list) flag)
      (cons (car list) (edit-sl (cdr list) flag)))))

;stifle-flag just reduces the range to a singleton.

(defun stifle-flag (flag)
  (let ((l current-sl))
    (if l (progn (setf (searchlist-list l) (edit-sl-2 (searchlist-list l) flag)) 
		 t) 
      (msg t "No such searchlist."))))

(defun edit-sl-2 (list flag)
  (if (null list) nil
    (progn
    (if (eq (search-item-flag (car list)) flag)
	(progn (setf (search-item-current (car list)) (eval (search-item-flag (car list))))
	       (setf (search-item-range (car list)) (list (search-item-current (car list))))
	       (setf (search-item-default (car list)) (search-item-current (car list)))))
    (cons (car list) (edit-sl-2 (cdr list) flag)))))

(deftest add-flag*
  (test-argnames )
  (test-argtypes )
  (mhelp "Repeatedly add new flags to the current searchlist."))

(deftest rem-flag*
  (test-argnames )
  (test-argtypes )
  (mhelp "Repeatedly remove flags from the current searchlist."))

(defun add-flag* ()
  (declare (special again))
  (eval (test-command-interpreter  '(add-flag)))
  (msg t)
  (prompt-read again nil (msgf "Add another flag? ") 'yesno 'yes nil)
  (do () ((not again))
      (msg t)
      (eval (test-command-interpreter '(add-flag)))
      (msg t t)
      (prompt-read again nil (msgf "Add another flag? ") 'yesno 'yes nil)))

(defun rem-flag* ()
  (declare (special again))
  (eval (test-command-interpreter  '(rem-flag)))
  (msg t)
  (prompt-read again nil (msgf "Remove another flag? ") 'yesno 'yes nil)
  (do () ((not again))
      (msg t)
      (eval (test-command-interpreter '(rem-flag)))
      (msg t t)
      (prompt-read again nil (msgf "Remove another flag? ") 'yesno 'yes nil)))

(deftest add-subjects
  (test-argnames subjects )
  (test-argtypes subjectlist )
  (mhelp "Add all the flags concerning the given subjects to the current 
searchlist."))

(defun add-subjects (sublist)
  (if (null current-sl) (msg "There is no current searchlist. Use NEW-SEARCHLIST first.")
    (dolist (subject sublist)
	    (msgf "Subject: " subject t)
	    (add-subject subject))))

(defun add-subject (subject)
  (declare (special again))
  (let ((subject-flags 
	 (sort (copy-list
		(remove-if-not 
		 #'(lambda (flag) (and (symbolp flag)
				       (member subject 
					       (get flag 'subjects))))
			       global-flaglist))
	       #'string<)))
  (dolist (flag subject-flags)
	  (prompt-read again nil (msg flag " takes objects of type " (get flag 'flagtype) t 
				      "Do you wish to add it to the searchlist? " ) 'yesno 'yes nil)
	  (if again 
	      (do () ((and again (eval (test-command-interpreter  (list 'add-flag flag))))) (prompt-read again nil (msg "Failed. Try adding flag again? ") 'yesno 'yes nil))))))

(context test-lib)

(deftest insert
  (test-argnames name type comment)
  (test-argtypes symbol lib-argtype string)
  (test-defaultfns (lambda (z y w)
		     (list (if (eq z '$) current-sl-name z)
			   (if (eq y '$) 'slist y)
			   (if (eq w '$) "" w))))
  (test-arghelp "Name of object" "Type" "Your choice" )
  (test-mainfns core::test-insert)
  (mhelp "Like the library command INSERT; will save a 
searchlist in the library. Will also save a mode that has been found 
by using GO."))

(deftest fetch
  (test-argnames name type)
  (test-argtypes symbol lib-argtype)
  (test-arghelp "Name of object" "Type (slist or mode)")
  (test-defaultfns (lambda (x y) (list x (if (eq y '$) nil y))))
  (test-mainfns core::test-fetch)
  (mhelp "Retrieve a searchlist or mode from the library. Exactly like the 
library function FETCH, except that when a searchlist is retrieved, it
will become the current searchlist."))

(deftest delete
  (test-argnames name type)
  (test-argtypes symbol lib-argtype)
  (test-arghelp "Name of object" "Type (slist or mode)")
  (test-defaultfns (lambda (x y) (list x (if (eq y '$) nil y))))
  (test-mainfns core::test-delete)
  (mhelp "Delete a saved searchlist or mode (equivalent to the library
command DELETE."))

(defun slist-to-list (name)
  (let ((sl (searchlist-list (find-searchlist name *global-searchlist*))))
    (if sl (s-to-l sl) nil)))

(defun s-to-l (sl)
  (if (null sl) nil
    (cons (list (search-item-flag (car sl)) (search-item-current (car sl)) (search-item-default (car sl)) 
		(search-item-range (car sl)) (search-item-fixed (car sl)) (search-item-internal (car sl)))
	  (s-to-l (cdr sl)))))

(defun list-to-slist (name list function)
  (new-searchlist name)
  (setf (searchlist-list current-sl) (l-to-s list))
  (setf (searchlist-function current-sl) function)
  (show-searchlist name)
  t)


(defun l-to-s (l)
  (if (null l) nil
    (cons (make-search-item :flag (car (car l))
			    :current (cadr (car l))
			    :default (caddr (car l))
			    :range (cadddr (car l))
			    :fixed (cadddr (cdr (car l)))
			    :internal (cadddr (cddr (car l)))) (l-to-s (cdr l)))))

(context test-searchlists) 

(deftest scale-down
  (test-argnames old-slist new-slist)
  (test-argtypes symbol symbol)
  (test-defaultfns (lambda (x y) (list (if (eq x '$) current-sl-name x)
				       (if (eq y '$) (intern (concatenate 'string (princ-to-string x) "-DOWN")) y))))
  (test-mainfns scale-down)
  (mhelp "Rewrites a searchlist under the assumption that the initial
values in the searchlist (together with appropriate settings of the other
flags) constitute a successful mode, and that TEST is being run in order 
to find a faster mode. This will discard all settings that would make the
search slower, and will arrange the range of values in such a way that the
bounds of the search will gradually decrease until the proof cannot be 
completed. If this makes the range empty or a singleton, the flag is removed
from the searchlist. See the TEST-FASTER-* flags"))

(deftest scale-up
  (test-argnames old-slist new-slist)
  (test-argtypes symbol symbol)
  (test-defaultfns (lambda (x y) (list (if (eq x '$) current-sl-name x)
				       (if (eq y '$) (intern (concatenate 'string (princ-to-string x) "-UP")) y))))
  (test-mainfns scale-up)
  (mhelp "Rewrites a searchlist under the assumption that the initial
values in the searchlist (together with appropriate settings of the other
flags) do not constitute a successful mode, and that TEST is being run in order
to find a mode that works. This will discard all settings that would make the 
search harder, and will arrange the range of values in such a way that the 
bounds of the search will gradually increase until the proof (with a bit of 
luck) can be completed. If this makes the range empty or a singleton, the flag 
is removed from the searchlist. See the TEST-EASIER-* flags."))

(deftest revise-defaults
  (test-argnames old-slist new-slist)
  (test-argtypes symbol symbol)
  (test-defaultfns (lambda (x y) (list (if (eq x '$) current-sl-name x)
				       (if (eq y '$) x y))))
  (test-mainfns revise-defaults)
  (mhelp "For each flag in the given searchlist, change the default setting to 
the current value of the flag, and put the default setting into the range
(unless it's already there). This is useful in conjunction with SCALE-UP 
and SCALE-DOWN; you can keep one searchlist (let's call it MASTER-SLIST) 
containing all of the flags you're likely to want to vary. Then if the 
current flag settings are a good mode and you want to try and find a better 
one, do REVISE-DEFAULTS followed by SCALE-DOWN MASTER-SLIST; if the current
settings are a bad mode and you want to try to find one that works, do 
REVISE-DEFAULTS followed by SCALE-UP MASTER-SLIST."))

(defun revise-defaults (old-slist new-slist)
  (initialise-scaling old-slist new-slist)
  (sorta-copy-slist old-slist new-slist))
	    
(defun sorta-copy-slist (o n)
  (let ((s (find-searchlist o *global-searchlist*))
	(newlist nil))
    (dolist (searchitem (searchlist-list s))
	    (push (make-search-item :flag (search-item-flag searchitem)
				    :current (eval (search-item-flag searchitem))
				    :default (eval (search-item-flag searchitem))
				    :range (union (list (eval (search-item-flag searchitem)))
						  (union (list (search-item-default searchitem))
							 (search-item-range searchitem)))
				    :fixed (search-item-fixed searchitem)
				    :internal (search-item-internal searchitem))
		  newlist))
    (setf (searchlist-list (find-searchlist n *global-searchlist*)) (nreverse newlist))
    t))

(defun initialise-scaling (old-slist new-slist)
  (if (not (known-searchlist-p old-slist)) (throwfail "Don't know the searchlist " old-slist t)) 
  (if (known-searchlist-string-p (princ-to-string new-slist))
      (if (query (concatenate 'string "The searchlist " (princ-to-string new-slist) " is already defined.
Overwrite it?") t)
	  (progn (setq current-sl (find-searchlist-string (princ-to-string new-slist) *global-searchlist*))
		 (setq current-sl-name (searchlist-name current-sl)))
	(throwfail t))
    (new-searchlist new-slist)))

(defun scale-down (old-slist new-slist)
  (initialise-scaling old-slist new-slist)
  (msg t)
  ;now we rearrange things
  (let ((new-searchlist-list nil)
	(replacement (string= (princ-to-string old-slist) (princ-to-string new-slist))))
    (dolist (searchitem (searchlist-list (find-searchlist old-slist *global-searchlist*)))
	    (let* ((flag (search-item-flag searchitem))
		   (default (search-item-default searchitem))
		   (range (search-item-range searchitem))
		   (shuffled-list (downward-search flag default range)))
	      (unless (or (null (cdr shuffled-list)) (null shuffled-list))
		      (if replacement
			  (progn (setf (search-item-range searchitem) shuffled-list)
				 (push searchitem new-searchlist-list))
			(setq new-searchlist-list 
			      (append new-searchlist-list 
				(list (make-search-item :flag flag
							:current (search-item-current searchitem)
							:default default
							:range (sort-ascending (get flag 'flagtype) shuffled-list)
							:fixed (search-item-fixed searchitem)
							:internal (search-item-internal searchitem)))))))))
    (setf (searchlist-list current-sl) new-searchlist-list)
    t))

(defun downward-search (flag default range)
  (let ((value (downward-search-real flag default range)))
    (if (memq default value) value (cons default value))))

(defun downward-search-real (flag default range)
  (let ((type (get flag 'flagtype)))
    (case type
	  (boolean (if (memq flag test-faster-if-nil)
		       (sort-descending type (remove-smaller type default range))
		     (if (memq flag test-faster-if-t)
			 (sort-ascending type (remove-bigger type default range))
		       range)))
	  (verbose (sort-ascending type (remove-smaller type default range))) ; always better to be quieter!
	  ((existing-line integer+ null-or-posinteger posinteger posnumber integer+-or-infinity posinteger-or-infinity)
	   (if (memq flag test-faster-if-high)
	       (sort-ascending type (remove-bigger type default range))
	     (if (memq flag test-faster-if-low)
		 (sort-descending type (remove-smaller type default range))
	       range)))
	  (t range))))

(defun scale-up (old-slist new-slist)
  (initialise-scaling old-slist new-slist)
  (msg t)
  ;now we rearrange things
  (let ((new-searchlist-list nil)
	(replacement (string= (princ-to-string old-slist) (princ-to-string new-slist))))
    (dolist (searchitem (searchlist-list (find-searchlist old-slist *global-searchlist*)))
	    (let* ((flag (search-item-flag searchitem))
		   (default (search-item-default searchitem))
		   (range (search-item-range searchitem))
		   (shuffled-list (upward-search flag default range)))
	      (unless (or (null (cdr shuffled-list)) (null shuffled-list))
		      (if replacement
			  (progn (setf (search-item-range searchitem) shuffled-list)
				 (push searchitem new-searchlist-list))
			(setq new-searchlist-list 
			      (append new-searchlist-list 
				(list (make-search-item :flag flag
							:current (search-item-current searchitem)
							:default default
							:range (sort-descending (get flag 'flagtype) shuffled-list)
							:fixed (search-item-fixed searchitem)
							:internal (search-item-internal searchitem)))))))))
    (setf (searchlist-list current-sl) new-searchlist-list)
    t))

(defun upward-search (flag default range)
  (let ((value (upward-search-real flag default range)))
    (if (memq default value) value (cons default value))))

(defun upward-search-real (flag default range)
  (let ((type (get flag 'flagtype)))
    (case type
	  (boolean (if (memq flag test-easier-if-nil)
		       (sort-descending type (remove-smaller type default range))
		     (if (memq flag test-easier-if-t)
			 (sort-ascending type (remove-bigger type default range))
		       range)))
	  (verbose (sort-ascending type (remove-smaller type default range))) ; always better to be quieter!
	  ((existing-line integer+ posinteger-or-infinity 
			  null-or-posinteger posinteger posnumber integer+-or-infinity)
	   (if (memq flag test-easier-if-high)
	       (sort-ascending type (remove-bigger type default range))
	     (if (memq flag test-easier-if-low)
		 (sort-descending type (remove-smaller type default range))
	       range)))
	  (t range))))

(defun remove-bigger (type default range)
  (remove-if #'(lambda (x) (not (type-lessthanequal type default x))) range))

(defun remove-smaller (type default range)
  (remove-if #'(lambda (x) (type-lessthanequal type default x)) range))

(defun sort-ascending (type range)
  (if (null range) nil
    (if (null (cdr range)) range
      (append (sort-ascending type (remove-bigger type (car range) (cdr range)))
	      (list (car range))
	      (sort-ascending type (remove-smaller type (car range) (cdr range)))))))

(defun sort-descending (type range)
  (if (null range) nil
    (if (null (cdr range)) range
      (append (sort-descending type (remove-smaller type (car range) (cdr range)))
	      (list (car range))
	      (sort-descending type (remove-bigger type (car range) (cdr range)))))))


(defun type-lessthanequal (type a b)
  (case type
	    (verbose (if (member b (member a '(nil silent min med max t)))
			 t ; nil<silent<min<med<max<t
		       nil))
	    (boolean (if (and (not a) b) t nil)) ; nil<t
	    ((existing-line integer+ posinteger posnumber)
	     (if (> a b) nil t)) ; ordinary <
	    (null-or-posinteger (if (null b) t (if (null a) nil (> a b))))
	    ;generally NIL = infinite...
	    ((posinteger-or-infinity integer+-or-infinity) (if (option-> b a) nil t)) ; > with infinity
	    (t nil))) ; shouldn't be here!
