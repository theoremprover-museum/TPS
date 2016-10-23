;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :AUTO)
;;;
;;; File TEST-TOP-SEARCH
;;;
;;; defines search fns of the test-top level.
;;;

(deffile test-top-search
  (part-of mating)
  (extension lisp)
  (mhelp "Defines the search procedures for test-top."))

(context mating-search)

(defun memb (elt list)
  (if (listp elt) 
      (if (null elt)
	  (if (memq nil list) t nil)
	(memb (cdr elt) (mapcar #'cdr (remove-if-not #'(lambda (x) (eq (car elt) (car x))) list))))
    (memq elt list)))
;membership relation for 

(defun exhaustive-search (sl &optional (improved nil))
  (declare (ignore improved))
  (let ((curr-list (searchlist-list sl))
	(exit-flag nil))
    (do ((next-item (car curr-list) (car curr-list))
	 (curr-list (cdr curr-list) (cdr curr-list)))
	((or (null next-item) exit-flag) 
	 (if (and (null next-item) (not exit-flag)) 
	     (progn (if test-verbose (msg "Search finished.")) (setq *finished-flag* t))
	   (setq *finished-flag* nil)))
	(let ((item-flag (search-item-flag next-item))
	      (item-range (search-item-range next-item))
	      (item-current (search-item-current next-item))
	      (item-internal (search-item-internal next-item)))
	  (when (null item-internal) 
		(setf (search-item-internal next-item) (list item-current))
		(setq item-internal (list item-current)))
	  (if (null (remove-if #'(lambda (x) (memb x item-internal)) item-range))
	      ;was (set-equal (adjoin item-current item-internal) item-range)
	      ; then we have gone through a full cycle on this item: go back to the first 
	      ; item on the list and also change the next item. If there is no next item, 
	      ; we're finished.
	      (let ((new-value (car item-range)))
		(set-flag item-flag new-value)
		(setf (search-item-internal next-item) (list new-value))
		(setf (search-item-current next-item) new-value)
		(if test-verbose (msg t "Changed " next-item t)))
	      ;otherwise we just pick one of the remaining values and try that...
	    (let ((new-value (car (remove-if #'(lambda (x) (memb x item-internal)) item-range))))
	      (set-flag item-flag new-value)
	      (setq exit-flag t)
	      (setf (search-item-internal next-item) (adjoin new-value item-internal))
	      (setf (search-item-current next-item) new-value)
	      (if test-verbose (msg t "Changed " next-item t))))))))

;idea of BFS: suppose we have flags A B C with values a0 a1 a2... b0 b1 b2... c0 c1 c2...
;arranged in some order. The default setting is a0 b0 c0. 
;The depth of a setting ai bj ck is (i+j+k).
;We try all settings of depth 1, then all of depth 2, and so on.

;enumerate as follows: at depth k, we have k tokens numbered 1,..,k
;1 is the "heaviest", k the "lightest". At each stage, the number of tokens on each flag is what counts.
;A) Start with everything on the first flag (a(123) b0 c0) --> (a3 b0 c0)
;B) Move the lightest token down to the end, in steps (a(12) b(3) c0) ---> a2 b1 c0
;                                                     (a(12) b0 c(3)) ---> a2 b0 c1
;C) When a token reaches the end, move the next-lightest token one step and then 
;    move all the lighter tokens back to it:          (a(1) b(23) c0) ---> a1 b2 c0
;D) Repeat B & C until all tokens reach the end. Add a new token and start again at depth k+1.

;We implement this by storing "1" through "k" in the search-item-internal slot of each search item.

(defun breadth-first-search (sl &optional (improved nil))
  (declare (ignore improved))
  (let ((curr-list (searchlist-list sl))
	(count 0))
    (dolist (item curr-list)
	    (setq count (+ count (1- (length (search-item-range item))))))
    (if (> *current-bfs-depth* count)
	(progn (if test-verbose (msg "Search finished.")) (setq *finished-flag* t))
      (progn
	(breadth-first-search-real sl)
	(do ((valid (valid-list curr-list) (valid-list curr-list))
	     (failed (> *current-bfs-depth* count) (> *current-bfs-depth* count)))
	    ((or failed valid) (if valid (progn (set-bfs-defaults sl) (assert-current sl))
				 (progn (if test-verbose (msg "Search finished."))
					(setq *finished-flag* t))))
	    (breadth-first-search-real sl))))))

(defun valid-list (sl)
  (let ((valid T))
    (dolist (item sl)
	    (if (>= (length (search-item-internal item)) (length (search-item-range item)))
		(setq valid nil)))
    valid))

(defun set-bfs-defaults (sl)
  (dolist (item (searchlist-list sl))
    ;set all of the current values to the new setting.
	  (setf (search-item-current item)
		(nth (length (search-item-internal item)) (search-item-range item))))
  (setq *finished-flag* nil))

(defun breadth-first-search-real (sl)
  (let* ((curr-list (searchlist-list sl))
	 (last-elt (car (reverse curr-list)))
	 (first-elt (car curr-list))
	 (tokens-at-last-flag (search-item-internal last-elt)))
    (if (or (= 0 *current-bfs-depth*)
	    (member *current-bfs-depth* tokens-at-last-flag))
	;we're at stage C
	(let ((already-there (car (sort tokens-at-last-flag #'<))))
	  (if (or (null already-there) 
		  (= 1 already-there))
	      ;then all the tokens are at the end, or there are no tokens.
	      ;either way, add another one and start again
	      (progn
		(incf *current-bfs-depth*)
		(dolist (item curr-list)
			(setf (search-item-internal item) nil))
		(setf (search-item-internal first-elt)
		      (do ((list nil (cons next list))
			   (next 1 (1+ next)))
			  ((> next *current-bfs-depth*) list))))
	    ;not all the tokens are at the end, just tokens from "already-there" onwards.
	    (progn (setf (search-item-internal last-elt) nil)
		   (do* ((list curr-list (cdr list))
			 (first-elt (car list) (car list))
			 (next-elt (cadr list) (cadr list)) 
					;we never reach the last elt - that was case above.
			 (curr-internal (search-item-internal first-elt) (search-item-internal first-elt)))
			((member (1- already-there) curr-internal) ;found it! (it must be there)
			 (setf (search-item-internal first-elt)
			       (delete (1- already-there) curr-internal)) ;removed it!
			 (setf (search-item-internal next-elt)
			       ;cadr exists because o/w already-there was at the end of the list...
			       (do ((list nil (cons next list))
				    (next (1- already-there) (1+ next)))
				   ((> next *current-bfs-depth*) list))) ; added to next item!
			 )))))
      ;otherwise we're at stage B, and all we have to do is move the lightest token.
      (do* ((list curr-list (cdr list))
	    (first-elt (car list) (car list))
	    (next-elt (cadr list) (cadr list))
	    (curr-internal (search-item-internal first-elt) (search-item-internal first-elt)))
	   ((member *current-bfs-depth* curr-internal) ;found it!
	    (setf (search-item-internal first-elt)
		  (delete *current-bfs-depth* curr-internal)) ;removed it!
	    (setf (search-item-internal next-elt)
					;cadr exists because o/w we were at stage C...
		  (list *current-bfs-depth*)))))))

;press-down adds fixed tokens to the ones that move about in bfs
;these are stored in *press-down-fixed* in the form (flag . no-of-fixed-tokens)
;the flags that were increased (i.e. got more tokens) are stored in *press-down-changed* and 
;are fixed when press-down is next called.

;push-up is just breadth-first search on a slist of increasing values, until 
;we find one that works.

(defun push-up (sl &optional (improved nil))
  (if (eq improved 0) (breadth-first-search sl improved)
    ;this is the case when the routine is called by search-order.
    (if improved (progn (msgf "Found a successful mode! Terminating search." t)
			(setq *finished-flag* t))
      ;if we succeeded, we can stop.
      (breadth-first-search sl nil))))

(defun push-up-2 (sl &optional (improved nil))
  (push-up sl improved))

(defun press-down-2 (sl &optional (improved nil))
  (press-down sl improved))

(defun press-down (sl &optional (improved nil))
  (if (eq improved 0) (progn (msgf "Can't do SEARCH-ORDER with PRESS-DOWN search." t)
			     (setq *finished-flag* t))
    (progn
    (if (and improved *press-down-changed*) (rewrite-fixed-list))
    (let ((curr-list (searchlist-list sl))
	  (count 0))
      (dolist (item curr-list)
	      (setq count (+ count (1- (length (search-item-range item))))))
      (if (> (+ (count-fixed-tokens) *current-bfs-depth*) count)
	  (progn (if test-verbose (msg "Search finished.")) (setq *finished-flag* t))
	(progn
	  (press-down-search-real sl)
	  (do ((valid (valid-press-down-list curr-list) (valid-press-down-list curr-list))
	       (failed (> (+ (count-fixed-tokens) *current-bfs-depth*) count) 
		       (> (+ (count-fixed-tokens) *current-bfs-depth*) count))
	       (failed2 (> *current-bfs-depth* (1+ *press-down-last-succeeded*))
			(> *current-bfs-depth* (1+ *press-down-last-succeeded*))))
	      ((or failed failed2 valid) 
	       (if (and valid (not failed2))
		   (progn (set-press-down-defaults sl) (assert-current sl))
		 (progn (if test-verbose (msg "Search finished."))
			(setq *finished-flag* t))))
	      (press-down-search-real sl))))))))

(defun count-fixed-tokens ()
  (let ((count 0))
    (dolist (item *press-down-fixed*)
	    (setq count (+ count (cdr item))))
    count))

(defun rewrite-fixed-list ()
  (dolist (item *press-down-changed*)
	  (let ((val (cdr (assoc item *press-down-fixed*))))
	    (if val
		(rplacd (assoc item *press-down-fixed*) (1+ val))
	      (setq *press-down-fixed* (cons (cons item 1) *press-down-fixed*)))))
  (setq *press-down-changed* nil))

(defun valid-press-down-list (sl)
  (let ((valid T))
    (dolist (item sl)
	    (if (>= (+ (length (search-item-internal item))
		       (or (cdr (assoc (search-item-flag item) *press-down-fixed*)) 0))
		    (length (search-item-range item)))
		(setq valid nil)))
    valid))

(defun set-press-down-defaults (sl)
  (dolist (item (searchlist-list sl))
    ;set all of the current values to the new setting.
	  (setf (search-item-current item)
		(nth (+ (length (search-item-internal item))
			(or (cdr (assoc (search-item-flag item) *press-down-fixed*)) 0))
		     (search-item-range item))))
  (setq *finished-flag* nil))

(defun press-down-search-real (sl)
  (let* ((curr-list (searchlist-list sl))
	 (last-elt (car (reverse curr-list)))
	 (first-elt (car curr-list))
	 (tokens-at-last-flag (search-item-internal last-elt)))
    (if (or (= 0 *current-bfs-depth*)
	    (member *current-bfs-depth* tokens-at-last-flag))
	;we're at stage C
	(let ((already-there (car (sort tokens-at-last-flag #'<))))
	  (if (or (null already-there) 
		  (= 1 already-there))
	      ;then all the tokens are at the end, or there are no tokens.
	      ;either way, add another one and start again
	      (progn
		(incf *current-bfs-depth*)
		(setq *press-down-changed* (list (search-item-flag (car curr-list))))
		(dolist (item curr-list)
			(setf (search-item-internal item) nil))
		(setf (search-item-internal first-elt)
		      (do ((list nil (cons next list))
			   (next 1 (1+ next)))
			  ((> next *current-bfs-depth*) list))))
	    ;not all the tokens are at the end, just tokens from "already-there" onwards.
	    (progn (setf (search-item-internal last-elt) nil)
		   (do* ((list curr-list (cdr list))
			 (first-elt (car list) (car list))
			 (next-elt (cadr list) (cadr list)) 
					;we never reach the last elt - that was case above.
			 (curr-internal (search-item-internal first-elt) (search-item-internal first-elt)))
			((member (1- already-there) curr-internal) ;found it! (it must be there)
			 (setf (search-item-internal first-elt)
			       (delete (1- already-there) curr-internal)) ;removed it!
			 (setq *press-down-changed* (make-list (+ 2 (- *current-bfs-depth* already-there))
							       :initial-element (search-item-flag next-elt)))
			 (setf (search-item-internal next-elt)
			       ;cadr exists because o/w already-there was at the end of the list...
			       (do ((list nil (cons next list))
				    (next (1- already-there) (1+ next)))
				   ((> next *current-bfs-depth*) list))) ; added to next item!
			 )))))
      ;otherwise we're at stage B, and all we have to do is move the lightest token.
      (do* ((list curr-list (cdr list))
	    (first-elt (car list) (car list))
	    (next-elt (cadr list) (cadr list))
	    (curr-internal (search-item-internal first-elt) (search-item-internal first-elt)))
	   ((member *current-bfs-depth* curr-internal) ;found it!
	    (setf (search-item-internal first-elt)
		  (delete *current-bfs-depth* curr-internal)) ;removed it!
	    (setq *press-down-changed* (list (search-item-flag next-elt)))
	    (setf (search-item-internal next-elt)
					;cadr exists because o/w we were at stage C...
		  (list *current-bfs-depth*)))))))

(deftest search-order
  (test-argnames name)
  (test-argtypes symbol)
  (test-arghelp "Name of searchlist")
  (test-defaultfns (lambda (name) (list (if (eq name '$) current-sl-name name))))
  (mhelp "Show the order in which things will be changed if the search is
started now using the given searchlist."))

(defun search-order (name)
  (let ((l (find-searchlist name *global-searchlist*))
	(test-verbose nil))
    (setq *current-bfs-depth* 0)
    (when l
	  (msg "There are " (count-flagvals (searchlist-list l)) " possible settings of these flags." t)
	  (assert-defaults l)
	  (setq *finished-flag* nil)
	  (msg "Search : " ) (list-flagvals l) (msg t)
	  (do () (*finished-flag*)
	      (funcall test-next-search-fn l 0) 
	      (if (not *finished-flag*) 
		  (progn (msg "Search : ") (list-flagvals l) (msg t))))
	  (assert-defaults l))
    t))

(defun count-flagvals (sl &optional (count 1))
  (if (null sl) count
    (count-flagvals (cdr sl) (* count (length (search-item-range (car sl)))))))

(defun list-flagvals (sl)
  (dolist (flag (searchlist-list sl))
	  (msg "(" (search-item-flag flag) " = " (search-item-current flag) ") ")))

(defun makelist-flagvals (sl)
  (let ((outlist nil))
    (dolist (flag (searchlist-list sl))
	    (setq outlist (cons (list (search-item-flag flag) (eval (search-item-flag flag))) outlist)))
    outlist))

(defun assert-defaults (sl)
  (dolist (flag (searchlist-list sl))
	  (setf (search-item-current flag) (search-item-default flag))
	  (setf (search-item-internal flag) nil)
	  (set-flag (search-item-flag flag) (search-item-default flag))))

(defun assert-current (sl)
  (dolist (flag (searchlist-list sl))
	  (if (search-item-current flag) (set-flag (search-item-flag flag) (search-item-current flag))
	    (set-flag (search-item-flag flag) (search-item-default flag)))))

(deftest go
  (test-argnames modename testwin)
  (test-argtypes symbol yesno)
  (test-arghelp "Name for optimal mode" "Open a window for test-top summary?")
  (test-defaultfns (lambda (x y) (list (if (eq x '$) *test-mode-name* x) 
				       (if (eq y '$) nil y))))
  (test-mainfns go-test)
  (mhelp "Start searching with current searchlist & current problem."))

(deftest exhaustive-search
  (test-argnames modename testwin)
  (test-argtypes symbol yesno)
  (test-arghelp "Name for optimal mode" "Open a window for test-top summary?")
  (test-defaultfns (lambda (x y) (list (if (eq x '$) *test-mode-name* x) 
				       (if (eq y '$) nil y))))
  (test-mainfns go-test-exhaustive-search)
  (mhelp "Equivalent to setting TEST-NEXT-SEARCH-FN to EXHAUSTIVE-SEARCH
and then typing GO. Permanently changes TEST-NEXT-SEARCH-FN."))

(defun go-test-exhaustive-search (a b)
  (set-flag 'test-next-search-fn 'exhaustive-search)
  (go-test a b))

(deftest breadth-first-search
  (test-argnames modename testwin)
  (test-argtypes symbol yesno)
  (test-arghelp "Name for optimal mode" "Open a window for test-top summary?")
  (test-defaultfns (lambda (x y) (list (if (eq x '$) *test-mode-name* x) 
				       (if (eq y '$) nil y))))
  (test-mainfns go-test-breadth-first-search)
  (mhelp "Equivalent to setting TEST-NEXT-SEARCH-FN to BREADTH-FIRST-SEARCH
and then typing GO. Permanently changes TEST-NEXT-SEARCH-FN."))

(defun go-test-breadth-first-search (a b)
  (set-flag 'test-next-search-fn 'breadth-first-search)
  (go-test a b))

(deftest push-up-2
  (test-argnames modename testwin)
  (test-argtypes symbol yesno)
  (test-arghelp "Name for optimal mode" "Open a window for test-top summary?")
  (test-defaultfns (lambda (x y) (list (if (eq x '$) *test-mode-name* x) 
				       (if (eq y '$) nil y))))
  (test-mainfns go-test-push-up-2)
  (mhelp "Equivalent to setting TEST-NEXT-SEARCH-FN to PUSH-UP-2
and then typing GO. Permanently changes TEST-NEXT-SEARCH-FN.
Note that this is NOT the same as typing PUSH-UP; this will
use the user-defined searchlist rather than an automatically
generated one."))

(defun go-test-push-up-2 (a b)
  (set-flag 'test-next-search-fn 'push-up-2)
  (go-test a b))

(deftest press-down-2
  (test-argnames modename testwin)
  (test-argtypes symbol yesno)
  (test-arghelp "Name for optimal mode" "Open a window for test-top summary?")
  (test-defaultfns (lambda (x y) (list (if (eq x '$) *test-mode-name* x) 
				       (if (eq y '$) nil y))))
  (test-mainfns go-test-press-down-2)
  (mhelp "Equivalent to setting TEST-NEXT-SEARCH-FN to PRESS-DOWN-2
and then typing GO. Permanently changes TEST-NEXT-SEARCH-FN.
Note that this is NOT the same as typing PRESS-DOWN; this will
use the user-defined searchlist rather than an automatically
generated one."))

(defun go-test-press-down-2 (a b)
  (set-flag 'test-next-search-fn 'press-down-2)
  (go-test a b))

(deftest continue
  (test-argnames modename testwin)
  (test-argtypes symbol yesno)
  (test-arghelp "Name for optimal mode" "Open a window for test-top summary?")
  (test-defaultfns (lambda (x y) (list (if (eq x '$) *test-mode-name* x) 
				       (if (eq y '$) nil y))))
  (test-mainfns go-test-b)
  (mhelp "Continue searching with current searchlist & current problem
(similar to GO, but will continue from the last point reached rather
than restarting at the beginning again)."))

(deftest press-down
  (test-argnames modename testwin)
  (test-argtypes symbol yesno)
  (test-arghelp "Name for mode to be created" "Open a window for test-top summary?")
  (test-defaultfns (lambda (x y) (list (if (eq x '$) *test-mode-name* x) 
				       (if (eq y '$) nil y))))
  (test-mainfns press-down-go)
  (mhelp "Before using this command, use the MODE command to 
set up a mode in which the current theorem can be proven. Also
check the value of the TEST-INITIAL-TIME-LIMIT flag (it should
be high enough that the first attempt at proof will succeed).
Then PRESS-DOWN will systematically vary the values of the flags
listed in the TEST-FASTER-IF-* flags, using the PRESS-DOWN search
function (see the help message for TEST-NEXT-SEARCH-FN).
The values of TEST-REDUCE-TIME, TEST-NEXT-SEARCH-FN and 
TEST-FIX-UNIF-DEPTHS will be permanently changed (to T, PRESS-DOWN
and T respectively).

Note that this is NOT the same as PRESS-DOWN-2, since it automatically
generates a searchlist rather than relying on the user to provide one."))

(deftest push-up
  (test-argnames modename testwin)
  (test-argtypes symbol yesno)
  (test-arghelp "Name for mode to be created" "Open a window for test-top summary?")
  (test-defaultfns (lambda (x y) (list (if (eq x '$) *test-mode-name* x) 
				       (if (eq y '$) nil y))))
  (test-mainfns push-up-go)
  (mhelp "This command effectively runs PUSH-UP until it finds
a mode that works, and then stops.
Before using this command, use the MODE command to 
set up a mode in which the current theorem can not be proven. Also
check the value of the TEST-INCREASE-TIME flag (it should probably
not be zero).
Then PUSH-UP will systematically vary the values of the flags
listed in the TEST-EASIER-IF-* flags, using the PUSH-UP search
function (see the help message for TEST-NEXT-SEARCH-FN).
The value of TEST-NEXT-SEARCH-FN will be changed to PUSH-UP.

Note that this is NOT the same as PUSH-UP-2, since it automatically
generates a searchlist rather than relying on the user to provide one"))

(deftest find-best-mode
  (test-argnames modename testwin)
  (test-argtypes symbol yesno)
  (test-arghelp "Name for mode to be created" "Open a window for test-top summary?")
  (test-defaultfns (lambda (x y) (list (if (eq x '$) *test-mode-name* x) 
				       (if (eq y '$) nil y))))
  (test-mainfns find-best-go)
  (mhelp "This command effectively runs PUSH-UP until it finds
a mode that works, and then runs PRESS-DOWN until it finds the
best mode it can.
Before using this command, use the MODE command to 
set up a mode in which the current theorem can not be proven. Also
check the value of the TEST-INCREASE-TIME flag (it should probably
not be zero).
Then PUSH-UP will systematically vary the values of the flags
listed in the TEST-EASIER-IF-* flags, using the PUSH-UP search
function (see the help message for TEST-NEXT-SEARCH-FN).
Once a correct mode is discovered, it will systematically vary
the values of the flags listed in the TEST-FASTER-IF-* flags,
using the PRESS-DOWN search function, until it finds as good a 
mode as it can.
The values of TEST-REDUCE-TIME, TEST-NEXT-SEARCH-FN, 
TEST-INCREASE-TIME and TEST-FIX-UNIF-DEPTHS will be permanently
changed."))

(defun go-test (modename window)
  (setq *times-around* 0)
  (startcount 'mating-ctr) (runcount 'mating-ctr)
  (if (eq test-next-search-fn 'press-down) (press-down-go modename window)
    (if (eq test-next-search-fn 'push-up) (push-up-go modename window)
      (if current-sl (test-matingsearch-controller modename nil window) 
	(msg "No current searchlist." t))))
  (breakcount 'mating-ctr)
  (display-time 'mating))

(defun press-down-go (modename window)
  (setq test-reduce-time T)
  (setq test-next-search-fn 'press-down)
  (setq test-fix-unif-depths T)
  (let ((test-max-search-values 50))
    (press-down-quick 'auto-press-down-slist))
  (if current-sl (test-matingsearch-controller modename nil window) (msg "No current searchlist." t)))

(defun push-up-go (modename window)
  (setq test-next-search-fn 'push-up)
  (let ((test-max-search-values 100))
    (push-up-quick 'auto-push-up-slist))
  (if current-sl (test-matingsearch-controller modename nil window) (msg "No current searchlist." t)))

(defun find-best-go (modename window)
  (setq test-next-search-fn 'push-up)
  (let ((test-max-search-values 100))
    (push-up-quick 'auto-push-up-slist))
  (if current-sl (test-matingsearch-controller modename nil window) (msg "No current searchlist." t))
  (press-down-go modename window))

(defun go-test-b (modename window)
  (if current-sl (test-matingsearch-controller modename t window) (msg "No current searchlist." t)))

;;;this function should be carefully written to handle all cases
(defun successful-output (output)
  (and output (not (eq output 'fail)) (not (eq output 'test-fail)) (not (eq output 'fail-inf))))

(defun least-search-depth-silent ()
  (declare (special *least-search-depth*))
  (unless first-order-mode-ms
        (msg t "The least needed unification tree depth is " *least-search-depth* ".")
	(set-flag 'max-search-depth *least-search-depth*)
	(set-flag 'max-utree-depth *least-search-depth*)))

(defun test-search (func modename cont)
  (declare (special *least-search-depth*))
  ;if modename is a list, we came from tps-test2, and the single element is a file that output goes to.
  (let ((succeeded-once nil)
	(bestmode-yet nil)
	(besttime-yet 0))
    (setq *finished-flag* nil)
    (setq *test-total-time* 0)
    (unless cont (progn (setq *current-bfs-depth* 0)
			(setq *press-down-fixed* nil)
			(setq *press-down-last-succeeded* 0)
			(setq *press-down-changed* nil)))
    (if cont (progn (msg "Continuing search with flagvals as follows:" t) (assert-current current-sl))
      (progn (assert-defaults current-sl) (msg "Changing flag settings as follows:" t)))
    (list-flagvals current-sl) (msg t)
    (do ((discard nil))
	(discard)
	(testwin-update-one (makelist-flagvals current-sl) (and (not *finished-flag*) modename))
	(if (eq query-user 'query-slists) 
	    (prompt-read discard nil (msgf "Search on these settings ? ") 'yesno nil nil)
	  (setq discard t))
	(unless discard
		(msgf "These settings were ignored." t)
		(testwin-update-three "These settings were ignored." t)
		(funcall test-next-search-fn current-sl nil)
		(msg "Changing flag settings as follows:" t)
		(list-flagvals current-sl) (msg t)))
    (testwin-update-one (makelist-flagvals current-sl) modename)
    (do ()
	(*finished-flag*)
	(setq *test-total-time* 0)
	(setq func (test-reread-func))
	(vpwin-test-update-one)
	(when (searchlist-function current-sl) (funcall (searchlist-function current-sl)))
	(if (successful-output (funcall (symbol-function func)))
	    (progn ;(breakcount 'mating)
		   (let ((runtime (/ (diffcount-internal-net 'mating) internal-time-units-per-second)))
		     (vpwin-test-update-two t (if (or (not succeeded-once) (< runtime besttime-yet)) 
						  (coerce runtime 'single-float) nil))
		     (testwin-update-two t (coerce runtime 'single-float) modename)
		     (if (and test-fix-unif-depths (not succeeded-once))
			 (unless (not *least-search-depth*) 
				 (least-search-depth-silent)
				 (msgf "Fixing unification depths to " *least-search-depth* " and removing flags." t)))
		     (setq *press-down-last-succeeded* *current-bfs-depth*)
		     ; this line above will be irrelevant unless we're in press-down
		     (when (and (symbolp modename) (or (not succeeded-once) (< runtime besttime-yet)))
			   (setq besttime-yet runtime)
			   (setq bestmode-yet (makelist-flagvals current-sl))
			   (testwin-update-three "This is the new best time."))
		     (when (and test-fix-unif-depths (not succeeded-once))
			   (progn (stifle-flag 'MAX-SEARCH-DEPTH)
				  (stifle-flag 'MAX-UTREE-DEPTH)))
		     (setq succeeded-once t)
		     (setq test-increase-time 0)
		     (when (and test-reduce-time (< (1+ (truncate runtime)) test-initial-time-limit)) 
			   (setq test-initial-time-limit (1+ (truncate runtime)))
			   (msg "Decreasing TEST-INITIAL-TIME-LIMIT." t)
			   (testwin-update-three "Decreasing TEST-INITIAL-TIME-LIMIT."))
		     (if (eq test-next-search-fn 'push-up-2)
			 (setq *finished-flag* t)
		       (do ((discard nil))
			   (discard)
			   (funcall test-next-search-fn current-sl (= besttime-yet runtime))
			   (msg "Changing flag settings as follows:" t)
			   (list-flagvals current-sl) (msg t)
			   (testwin-update-one (makelist-flagvals current-sl) (and (not *finished-flag*) modename))
			   (if (eq query-user 'query-slists) 
			       (prompt-read discard nil (msgf "Search on these settings ? ") 'yesno nil nil)
			     (setq discard t))
			   (unless discard 
				   (msgf "These settings were ignored." t)
				   (testwin-update-three "These settings were ignored." t))))))
	  (progn ;(breakcount 'mating)
		 (vpwin-test-update-two nil nil)
		 (testwin-update-two nil nil modename)
		 (do ((discard nil))
		     (discard)
		     (funcall test-next-search-fn current-sl nil)
		     (msg "Changing flag settings as follows:" t)
		     (list-flagvals current-sl) (msg t)
		     (testwin-update-one (makelist-flagvals current-sl) (and (not *finished-flag*) modename))
		     (if (eq query-user 'query-slists) 
			 (prompt-read discard nil (msgf "Search on these settings ? ") 'yesno nil nil)
		       (setq discard t))
		     (unless discard 
			     (msgf "These settings were ignored." t)
			     (testwin-update-three "These settings were ignored." t)))))
	(unless (or succeeded-once (= test-increase-time 0))
		(setq test-initial-time-limit (floor (* (+ 100 test-increase-time) test-initial-time-limit) 100))
		(msg "Increased TEST-INITIAL-TIME-LIMIT to " test-initial-time-limit "." t)
		(testwin-update-three "Increased TEST-INITIAL-TIME-LIMIT to " test-initial-time-limit "." t))
	(unless (and succeeded-once (or (eq test-next-search-fn 'push-up-2) *finished-flag*))
					; don't expunge if we're about to finish
		(progn (expunge)
		       (test-wff-prefix *test-gwff* t nil)))
					; reinitialise everything as though we just entered the top level
	)
    (if succeeded-once (progn (msg "Finished varying flags; succeeded in proof." t)
			      (when (symbolp modename)
				    (msg "Best mode was " bestmode-yet t)
				    (msg "Best time was " (coerce besttime-yet 'single-float) t)
				    (vpwin-test-update-three bestmode-yet))
			      (testwin-update-three "Finished varying flags; succeeded in proof." t )
			      (when (symbolp modename)
				    (testwin-update-three "Best mode was " bestmode-yet t
							  "Best time was " (coerce besttime-yet 'single-float) t
							  "Defined a mode called " modename t))
			      (when (symbolp modename)
				    (let ((flags (cons 'flag-settings (find-flagvals bestmode-yet))))
				      (eval `(defmode ,modename ,flags 
					       (mhelp "A mode resulting from a test-search."))))
				    (eval `(shadowing-import ',modename 'user))
				    (msg t "Have defined a mode called " modename "; use INSERT to put this into the library." t))
			      (when (eq test-next-search-fn 'push-up-2) t))
      (progn (msg "Didn't succeed in proving the theorem." t t t t 
		  "Doubling TEST-INITIAL-TIME-LIMIT and trying again." t)
	     (setq *current-bfs-depth* 0)
	     (setq *press-down-fixed* nil)
	     (setq *press-down-changed* nil)
	     (incf *times-around*)
	     (setq test-initial-time-limit (* 2 test-initial-time-limit))
	     (vpwin-test-update-three nil)
	     (testwin-update-three "Failed to prove theorem." t  
				   "Doubling TEST-INITIAL-TIME-LIMIT and trying again." t 
				   "New value is : " test-initial-time-limit t t t )
	     (test-search func modename nil)))))

(defun find-flagvals (bestmode-yet)
  (dolist (f bestmode-yet)
	  (set-flag (car f) (cadr f)))
  (let ((write-flags nil))
    (dolist (subject (cons default-ms '(unification important primsubs mating-search test-top)))
	    (dolist (flag global-flaglist)
		    (when (and (symbolp flag) (not (get flag 'synonym))
			       (member subject (get flag 'subjects)))
			  (unless (assoc flag write-flags)
				  (push (cons flag (list (eval flag))) write-flags)))))
    (dolist (f bestmode-yet)
	    (unless (assoc (car f) write-flags)
		    (push f write-flags)))
    (core::flagsort write-flags)))

(defun test-ms88-controller ()
  (declare (special val flag))
  (setq *test-total-time* (+ test-initial-time-limit *test-total-time*))
  (if ms-split (setq val (test-ms-top (eproof-etree current-eproof) (get-net-internal-run-time)))
      (multiple-value-setq (flag val) (ms (eproof-etree current-eproof) t test-initial-time-limit)))
  (when val (setq active-mating val))
 val)

(defun test-ms90-3-controller ()
  (setq *test-total-time* (+ test-initial-time-limit *test-total-time*))
  (ms90-3-controller (eproof-etree current-eproof) test-initial-time-limit))

(defun test-auto-search-ms90-9 ()
  (auto-search-ms90-9))

(defun test-ms92-9-controller ()
  (setq *test-total-time* (+ test-initial-time-limit *test-total-time*))
  (ms92-9-controller (eproof-etree current-eproof) test-initial-time-limit))

(defun test-auto-search-ms93-1 ()
  (auto-search-ms93-1))

(defun test-auto-search-ms91-7 ()
  (auto-search-ms91-7))

(defun test-matingsearch-controller (modename cont window &rest ignore)
   (declare (ignore ignore) (special *unif-stats-store* ms88-unif-counter))
   (if window (open-testwin-auto))
   (testwin-update-three t t t "**************" t t "Starting a new search." t 
			 "Theorem : " *test-gwff-name* t
			 "Statement : " *test-gwff* t
			 last-mode-name t t
			 "TEST-INITIAL-TIME-LIMIT is set to " 
			 test-initial-time-limit t t ) 
   (setq *unif-stats-store* nil)
   (setq ms88-unif-counter 0)
   (unwind-protect  
      (test-matingsearch-controller-real modename cont)
))

(defun test-matingsearch-controller-real (modename cont &rest ignore)
  (declare (ignore ignore) (special num-of-splitting))
  (setq num-of-splitting 0)
  (prog1
   (case default-ms
    (ms88 (when (test-search 'test-ms88-controller modename cont) (expand-etree-count)))
    (ms89 (when (test-search 'auto-search-ms89 modename cont) (expand-etree-count)))
    (ms90-3 (when (test-search 'test-ms90-3-controller modename cont) (expand-etree-count)))
    (ms90-9 (when (test-search 'test-auto-search-ms90-9 modename cont) (expand-etree-count)))
    (ms91-6 (when (test-search 'auto-search-ms91-6 modename cont) (expand-etree-count)))
    (ms91-7 (when (test-search 'test-auto-search-ms91-7 modename cont) (expand-etree-count)))
    (ms92-9 (when (test-search 'test-ms92-9-controller modename cont) (expand-etree-count)))
    (ms93-1 (when (test-search 'test-auto-search-ms93-1 modename cont) (expand-etree-count)))
    (ms98-1 (when (test-search 'matingsearch-controller modename cont) (expand-etree-count)))
    (ms03-7 (when (test-search 'ext-matingsearch-controller-test modename cont) (expand-etree-count)))
    (ms04-2 (when (test-search 'ms04-controller modename cont) (expand-etree-count)))
    (otherwise
     (msgf "DEFAULT-MS must take as values only mating search
procedures.  Possible values are MS88, MS89, MS90-3, MS90-9, MS91-6,
MS91-7, MS92-9, MS93-1, MS98-1, MS03-7 and MS04-2.")))))

(defun expand-etree-count ()
  ;we always call this, because the searchlist may have changed default-ms
  (when (and (eq test-next-search-fn 'push-up-2) (memq default-ms '(ms90-3 ms90-9 ms91-7 ms92-9 ms93-1 ms98-1 ms03-7 ms04-2)))
	(runcount 'mating-ctr)
	(expand-etree)
	(breakcount 'mating-ctr)))

(defun test-reread-func ()
  (let ((func 'test-ms88-controller))
    (case default-ms
	(ms88 (setq func 'test-ms88-controller))
	(ms89 (setq func 'auto-search-ms89))
	(ms90-3 (setq func 'test-ms90-3-controller))
	(ms90-9 (setq func 'test-auto-search-ms90-9))
	(ms91-6 (setq func 'auto-search-ms91-6))
	(ms91-7 (setq func 'test-auto-search-ms91-7))
	(ms92-9 (setq func 'test-ms92-9-controller))
	(ms93-1 (setq func 'test-auto-search-ms93-1))
	(ms98-1 (setq func 'matingsearch-controller))
	(ms03-7 (setq func 'ext-matingsearch-controller-test))
	(ms04-2 (setq func 'ms04-controller))
	(otherwise
	 (msgf "DEFAULT-MS must take as values only mating search
procedures.  Possible values are MS88, MS89, MS90-3, MS90-9, MS91-6,
MS91-7, MS92-9, MS93-1, MS98-1, MS03-7 and MS04-2.")))
    func))

(defun test-ms-top (etree start-time)
  (declare (special num-of-splitting))
  (if (etree-components* etree)
      (if (cdr (etree-components* etree))
	  (case (etree-junctive* etree)
	    (dis
	      (msg t "Splitting the problem.")
              (incf num-of-splitting)
	      (compose-matings
		(progn (msg t "Considering etree: "
			    (etree-name* (car (etree-components* etree))))
		       (test-ms-top (car (etree-components* etree)) start-time))
		(progn (decf num-of-splitting)
                       (msg t "Considering etree: "
			    (etree-name* (cadr (etree-components* etree))))
		       (test-ms-top (cadr (etree-components* etree)) start-time))))
	    (con (test-ms88-sub etree start-time))
	    (t (throwfail "No action specified for junctive property "
			  (etree-junctive etree) ". Node "
			  (etree-name etree))))
	  (if (expansion-p etree)
	      (if first-order-mode-ms (test-ms88-sub etree start-time t)
		  (let ((mating (test-ms88-sub etree start-time nil))
			(var (car (etree-components* etree))))
		    (if mating mating
			(progn
			  (msg t "Considering primitive substitutions "
			       "for variable: "(bdvar (get-shallow etree)))
			  (apply-prim-subs-outer etree)
			  (do ((comps (etree-components* etree) (cdr comps))
			       (terms (sel-exp-terms etree) (cdr terms))
			       (son))
			      ((null terms))
			    (unless (eq (car comps) var)
			      (msg t 3 "Primitive Substitution: "
				   ((car terms) . gwff))
			      (vpwin-update-seven (car terms))
			      (setq son (deepen-to-literals* (car comps)))
			      (setq mating (test-ms88-sub son start-time nil))
			      (if (>= (truncate (/ (- (get-net-internal-run-time) start-time) internal-time-units-per-second))
				      test-initial-time-limit)
				  (return-from test-ms-top nil))
			      (if mating (return-from test-ms-top mating)
				(update-status nil son 0))))
			  (let ((dup-strategy
				  (if (eq query-user T)
				      (if (query "Duplication required. Continue?"
						 '$)
					  duplication-strategy 'abort)
				      (progn (msg T "Duplicating")
					     duplication-strategy))))
			    (case dup-strategy
			      (dup-outer (duplicate-all-outer-vars etree))
			      (dup-all (duplicate-all-vars etree))
			      (t (throwfail "Search aborted."))))
			  (setq etree (deepen-after-duplication etree))
			  (test-ms88-sub etree start-time t)))))
	      (test-ms-top (car (etree-components* etree)) start-time)))
      (test-ms88-sub etree start-time t)))

(defun test-ms88-sub (&optional (etree-name nil) (start-time 0) (allow-duplications t))
  (declare (ignore start-time allow-duplications))
  (let ((etree (if etree-name
		   (if (etree-p etree-name) etree-name
		       (find-etree-node-name
			 etree-name (eproof-etree current-eproof)))
		   current-topnode)))
    (if etree
	(let ((mating-list (mating-list))
	      (max-incomp-clists-wrt-etree (max-incomp-clists-wrt-etree))
	      (incomp-clists-wrt-etree (incomp-clists-wrt-etree))
	      (incomp-clists (incomp-clists))
	      (duplicated nil)
	      (success nil)
	      (bktrack-limit (eproof-bktrack-limit current-eproof))
	      active-mating)
	  (declare (special active-mating duplicated flag give-up))
	  (unwind-protect
	      (progn (setf (mating-list) (list (init-mating)))
		     (setf (max-incomp-clists-wrt-etree) nil)
		     (setf (incomp-clists-wrt-etree) nil)
		     (setf (incomp-clists) nil)
		     (setf (eproof-bktrack-limit current-eproof)
			   initial-bktrack-limit)
		     (setq active-mating
			   (if (free-vars-in-etree current-eproof)
			       (ms etree t test-initial-time-limit)
			     (ms-propositional etree)))
		     (if (mating-p active-mating) (setq success t) (setq success nil))
		     ;(if success (setq active-mating (car mating-list)) (setq active-mating nil))
		     (unless success (setq active-mating nil))
		     active-mating)
	    (setf (mating-list) mating-list)
	    (setf (max-incomp-clists-wrt-etree) max-incomp-clists-wrt-etree)
	    (setf (incomp-clists-wrt-etree) incomp-clists-wrt-etree)
	    (setf (incomp-clists) incomp-clists)
	    (setf (eproof-bktrack-limit current-eproof)
		  bktrack-limit)
	    (when success (push active-mating (mating-list)))))
	(complain etree-name " not found in current-eproof."))))


;;****window handling****

(defun vpwin-test-update-one ()
  (when (and *vpwin-opened* (streamp *vpwin-opened*) (open-stream-p *vpwin-opened*))
	(let ((*standard-output* *vpwin-opened*)
	      (style (if use-window-style window-style style))
	      (rightmargin vpw-width))
	  (terpri)
	  (terpri)
	  (msg t "Current Testtop Flag Values:" t) (list-flagvals current-sl) (terpri)
	  (finish-output *standard-output*))))

(defun vpwin-test-update-two (success newbest)
  (when (and *vpwin-opened* (streamp *vpwin-opened*) (open-stream-p *vpwin-opened*))
	(let ((*standard-output* *vpwin-opened*)
	      (style (if use-window-style window-style style))
	      (rightmargin vpw-width))
	  (terpri)
	  (terpri)
	  (if success (msg "Succeeded in proving theorem in " newbest " seconds." t) (msg "Failed to finish proof." t))
	  (finish-output *standard-output*))))

(defun vpwin-test-update-three (bestmode-yet)
  (when (and *vpwin-opened* (streamp *vpwin-opened*) (open-stream-p *vpwin-opened*))
	(let ((*standard-output* *vpwin-opened*)
	      (style (if use-window-style window-style style))
	      (rightmargin vpw-width))
	  (terpri) (terpri)
	  (if bestmode-yet
	      (msg "Succeeded in proving theorem. Best flag settings were: " t bestmode-yet t)
	    (msg "Failed to prove theorem. Increasing time limit and trying again." t))
	  (finish-output *standard-output*))))

(defun open-testwin-auto ()
  (declare (special filename))
  (progn
    (prompt-read filename nil (msgf "File to send test-top summary to (\"\" to discard)") 'filespec "info.test" nil) 
    (when (not *testwin-opened*) 
	  (msg t "Use CLOSE-TESTWIN when you want to close the testwindow." t)
	  (when (and (neq filename "") (probe-file filename))
		(rename-file filename (concatenate 'string filename ".bak"))))
    (open-testwin filename)))

(deftest open-testwin
  (test-argtypes filespec)
  (test-argnames filename)
  (test-arghelp "File to send test-top summary to (\"\" to discard)")
  (test-defaultfns
    (lambda (filename) (list (if (eq filename '$) "info.test" filename))))
  (mhelp "Open a window which will display a summary of the test-top output.
The window can be closed with the command CLOSE-TESTWIN. The size 
of the text is determined by the flag CHARSIZE, and the current width of the window 
by the flag TESTWIN-WIDTH. The initial height of the window is determined by 
TESTWIN-HEIGHT."))

(defun open-testwin (&optional (filename ""))
  (if *testwin-opened* (if *current-testname* 
			 (msg t "Output is already going to an existing testwindow and to the file " *current-testname* "." t)
		       (msg t "A testwindow is already open, and output is being directed to that. Use CLOSE-TESTWIN if you no longer need it." t))
    (progn
    (if (equal charsize 'MAX)
	(setq *big-testwin* T)
      (setq *big-testwin* nil))
    (setq *testwin-opened* (setup-testwin filename))
    (if (string-equal filename "") (setq *current-testname* nil)))))

(deftest close-testwin
  (test-argtypes)
  (test-argnames)
  (mhelp "Closes the window that displays the test-top summary.
Use ..../tps/utilities/vpshow (from a shell, not from TPS) 
to view the output file again."))


(defmexpr close-testwin
  (argtypes)
  (argnames)
  (mainfns close-testwin)
  (mhelp "Closes the window that displays the test-top and TPS-TEST summary.
Use ..../tps/utilities/vpshow (from a shell, not from TPS) 
to view the output file again."))

(defun close-testwin ()
  (declare (special discard))
  (if *testwin-opened*
      (progn
      (close *testwin-opened*)
      (core::kill-xterm-window *testwin-process*)
      (if *current-testname* (progn (msg "Closed test-window file : " *current-testname*)
				   (prompt-read discard nil (msgf "Shall I delete the output file " *current-testname* "? ") 'yesno nil nil)
				   (if discard (delete-file *current-testname*))
				   (setq *current-testname* nil))) 
      (setq *testwin-opened* nil))))

(defun setup-testwin (fname)
  (let* ((outfilename (if (string-equal fname "")
			  (concatenate 'string "/tmp/foo" (princ-to-string (get-internal-run-time)))
			(namestring (merge-pathnames fname (make-pathname% :name "testinfo" :type "test")))))
	 (outstream (open outfilename :direction :output 
			  :if-exists :supersede
			  :if-does-not-exist :create)))
    (setq *current-testname* outfilename)
    (if *big-testwin*
	(setq *testwin-process* (core::setup-big-xterm-window "Test-Top Summary" outfilename 
						      (concatenate 'string (princ-to-string testwin-width)
								   "x" (princ-to-string testwin-height) "+0+0")))
      (setq *testwin-process* (core::setup-xterm-window "Test-Top Summary" outfilename 
						      (concatenate 'string (princ-to-string testwin-width)
								   "x" (princ-to-string testwin-height) "+0+0"))))
    outstream))

(defun testwin-update-one (list &optional (full nil))
  (when *testwin-opened*
	(let ((*standard-output* *testwin-opened*)
	      (style (if use-window-style window-style style))
	      (rightmargin testwin-width))
	  (msg t t "Search these flag values:" t list t)
	  (finish-output *standard-output*)))
  (when (and full (listp full))
	(reroute-output-append (car full) *default-pathname-defaults*
			       (msg t t "Search these flag values:" t list t))))


(defun testwin-update-two (succ time &optional (full nil))
  (when *testwin-opened*
	(let ((*standard-output* *testwin-opened*)
	      (style (if use-window-style window-style style))
	      (rightmargin testwin-width))
	  (if succ (msg "Succeeded in time : " (coerce time 'single-float) t)
	    (msg "Failed to find proof." t))
	  (finish-output *standard-output*)))
  (when (and full (listp full))
	(if succ (reroute-output-append (car full) *default-pathname-defaults*
					(msgf t "Timing information ")
					(auto::display-time 'auto::mating))
	  (reroute-output-append (car full) *default-pathname-defaults*
				 (msgf t t "Failed to find proof" t t))))) 


(defun testwin-update-three (&rest rest)
  (when *testwin-opened*
	(let ((*standard-output* *testwin-opened*)
	      (style (if use-window-style window-style style))
	      (rightmargin testwin-width))
	  (dolist (item rest) (if (string-equal (string-upcase (princ-to-string item)) "T") (msg t) 
				(if (gwff-q item) (msg (item . gwff)) (msg item))))
	  (finish-output *standard-output*))))
  
(defun uniform-search-function ()
  (declare (special max-substs-quick max-substs-var max-search-limit search-time-limit max-prim-depth))
  (setq max-utree-depth max-search-depth)
  (msgf "Setting MAX-UTREE-DEPTH to " MAX-UTREE-DEPTH t)
  (testwin-update-three "Setting MAX-UTREE-DEPTH to " MAX-UTREE-DEPTH t)
  (setq max-substs-quick max-substs-var)
  (msgf "Setting MAX-SUBSTS-QUICK to " MAX-SUBSTS-QUICK t)
  (testwin-update-three "Setting MAX-SUBSTS-QUICK to " MAX-SUBSTS-QUICK t)
  (setq max-search-limit search-time-limit)
  (msgf "Setting MAX-SEARCH-LIMIT to " MAX-SEARCH-LIMIT t)
  (testwin-update-three "Setting MAX-SEARCH-LIMIT to " MAX-SEARCH-LIMIT t)
  (setq MAX-PRIM-DEPTH (1+ *times-around*))
  (msgf "Setting MAX-PRIM-DEPTH to " MAX-PRIM-DEPTH t)
  (testwin-update-three "Setting MAX-PRIM-DEPTH to " MAX-PRIM-DEPTH t)
  (if (memq default-ms '(ms88 ms90-3))
      (progn (setq test-initial-time-limit (* (1+ *times-around*) max-search-limit))
	     (msgf "Allowing " test-initial-time-limit " seconds for this search.")
	     (testwin-update-three "Allowing " test-initial-time-limit " seconds for this search."))
    (progn
      (setq test-initial-time-limit (* (* 5 (1+ *times-around*)) max-search-limit))
      (msgf "Allowing time for " (* 5 (1+ *times-around*)) " option sets (" test-initial-time-limit " seconds)" t)
      (testwin-update-three "Allowing time for " (* 5 (1+ *times-around*)) " option sets (" 
			    test-initial-time-limit " seconds)" t))))

(defun basic-search-then-uniform-search ()
  (declare (special max-substs-quick max-substs-var))
  (if (zerop *times-around*)
	  ;first run through everything in basic-slist, then change to uniform-search-2
      (progn 
        (setq max-utree-depth max-search-depth)
	(msgf "Setting MAX-UTREE-DEPTH to " MAX-UTREE-DEPTH t)
	(testwin-update-three "Setting MAX-UTREE-DEPTH to " MAX-UTREE-DEPTH t)
	(setq max-substs-var 6 max-substs-quick 6)
	(msgf "Setting MAX-SUBSTS-QUICK and MAX-SUBSTS-VAR to 6" t)
	(testwin-update-three "Setting MAX-SUBSTS-QUICK and MAX-SUBSTS-VAR to 6" t)
	(setq test-initial-time-limit 3600)
	(msgf "Allowing 1 hour for search." t)
	(testwin-update-three "Allowing 1 hour for search."))
    (progn (unless (known-searchlist-p 'uniform-search-2)
		   (core::retrieve-libobject 'cl-user::uniform-search-2 :type 'slist :multiple nil))
	   (new-searchlist 'uniform-search-2)
	   (msgf "Basic searchlist failed. Switching to searchlist UNIFORM-SEARCH-2." t)
	   (testwin-update-three "Basic searchlist failed. Switching to searchlist UNIFORM-SEARCH-2." t)
	   (assert-defaults current-sl)
	   (setq *times-around* 0)
	   (uniform-search-function))))

