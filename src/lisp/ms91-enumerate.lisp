;;; -*- Mode:LISP; Package:AUTO -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1991 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)
(context ms91)

(deffile ms91-enumerate
  (part-of ms91)
  (mhelp "Functions dealing with enumeration of option-sets for use in
mating-search procedures MS91-6 and MS91-7."))

;;; *option-search-markers* should be a list of options,
;;; constituting a  list of markers from left to right in *option-list*

(defvar *option-search-markers* nil)
(defvar *option-set-weight-limit* 0)

(defvar *min-weight-d-on-this-round* infinity
"Keeps track of the minimum weight we have found so far on this round of
looking for a good option-set.")

(defun get-next-option-set ()
  (let ((temp-markers
	 (get-extended-option-set *option-search-markers* 
				  *option-set-weight-limit*)))
  (when (car temp-markers)
    (setq *option-search-markers* (car temp-markers))
    (cdr temp-markers))))
;    (options-to-option-set *option-search-markers*))))

(defflag ms91-time-by-vpaths
  (flagtype boolean)
  (default nil)
  (subjects ms91-6 ms91-7 transmit)
  (mhelp "When T, the amount of time given by SEARCH-TIME-LIMIT and 
MAX-SEARCH-LIMIT will be multiplied by the number of vertical paths
through the vpform and then divided by the number of paths through 
the initial vpform (so the first vpform will get SEARCH-TIME-LIMIT
seconds, and if the next has twice as many paths it will get twice
as many seconds, and so on...).
When NIL, every option set will get the same search time.
This flag only applies in MS91 procedures."))

(defflag ms91-prefer-smaller
  (flagtype boolean)
  (default t)
  (subjects ms91-6 ms91-7 transmit)
  (mhelp "When T, smaller option-sets will be preferred to any larger ones."))

(defflag ms91-weight-limit-range
  (flagtype integer+-or-infinity)
  (default 1) ;was 3
  (subjects ms91-6 ms91-7 transmit)
  (relevancy-preconditions (default-ms (eq default-ms 'ms91-6)))
  (irrelevancy-preconditions (default-ms (neq default-ms 'ms91-6)))
  (mhelp "New option-sets, when constructed, will be accepted if their weights lie
in the range [current weight limit, current weight limit + MS91-WEIGHT-LIMIT-RANGE].
Hence increasing this value means that more option-sets will be acceptable during 
the creation stage. If this range is very small, there is a risk that no option
sets at all will be accepted and the search will waste time recreating these sets
with a higher current weight limit. If it is too large, then there is a risk that
high-weighted sets will be considered before lower-weighted ones.
Note: option sets of weight INFINITY will never be accepted, no matter what."))

;;; Find first set that is of weight in range 
;;; [weight-limit, weight-limit + ms91-weight-limit-range) that
;;; is lexicographically greater than l. If there is none, return nil.

;;; changed to [ ... , ... ], since ms91-weight-limit-range can have value 0. MB 6/22/93

(defun get-extended-option-set (l weight-limit)
  (if options-verbose (msg "Looking for the first set in the range " weight-limit " to " (option-+ weight-limit ms91-weight-limit-range) "." t))
  (do ((max (option-+ weight-limit ms91-weight-limit-range))
       (next (if ms91-prefer-smaller
		 (get-next-set-in-lex-smaller-order l)
	       (get-next-set-in-lex-order l))
	     (if ms91-prefer-smaller
		 (get-next-set-in-lex-smaller-order next)
		 (get-next-set-in-lex-order next)))
       (new-oset nil nil))
      ((or (null next) 
	   (and next 
		(let ((next-weight-d 1))
		  (setq new-oset (options-to-option-set-2 next))
		  (setq next-weight-d (option-set-weight-d new-oset))
		  ;it seems that options are sorted by weight-a, so if weight-a isn't a 
		  ;major factor in weight-d then option sets get omitted (by the time the set is
		  ;generated, weight-limit is too high to allow it to be used). MB 2/94
		  (if options-verbose (msg "Min weight on this round : " 
					   *min-weight-d-on-this-round* 
					   " Weight of current set : " next-weight-d t))
		  (when (and (option-> *min-weight-d-on-this-round* next-weight-d)
			     (option->= *min-weight-d-on-this-round* weight-limit))
			(setq *min-weight-d-on-this-round* next-weight-d))
		  (or (and (option-= next-weight-d max)
			   (not (option-= next-weight-d infinity)))
		      (and (option->= next-weight-d weight-limit)
			   (option-> max next-weight-d))))))
       (list next new-oset))))
 
;;; Find the lexicographically next set, really sequence, after l
;;; Return nil if l is the lexicographically greatest element.
;;; Example of the ordering: from (1 2 3) we get in order 
;;; (1) (1 2) (1 2 3) (1 3) (2) (2 3) (3)
(defun get-next-set-in-lex-order (l)
  (cond 
   ;; empty sequence is first, then comes singleton first element of
   ;; *option-list*
   ((null l)
    (if *option-list*
	(list (car *option-list*))))
   ;; if l is a singleton, see if anything follows it in *option-list*
   ;; if so, add the first thing after to it and return
   ;; otherwise, it's the last thing in *option-list*, hence can't be
   ;; anything after it.
   ((null (cdr l))
    (let ((bigger-opts (cdr (member (car l) *option-list*))))
      (if bigger-opts
	  (append l (list (car bigger-opts)))
	nil)))
   ;; l has at least two items.  Find the next sequence in order after
   ;; the cdr of l, if there is one, and stick the car of l onto it.
   ;; if there isn't a next sequence after cdr of l, then get the 
   ;; next elt after the car l, if there is one.
   (t (let ((next-bigger-tail (get-next-set-in-lex-order (cdr l))))
	(if next-bigger-tail
	    (cons (car l) next-bigger-tail)
	  (let ((bigger-opts (cdr (member (car l) *option-list*))))
	    (if bigger-opts
		(list (car bigger-opts))
	      nil)))))))


;;; Here the ordering is used as above, but with a smaller set always
;;; coming before a larger one, i.e., from (1 2 3) we get
;;; in order (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3)
(defun get-next-set-in-lex-smaller-order (l)
  (let ((length-l (length l)))
    (cond 
     ;; l is already the entire *option-list*
     ((= *length-of-option-list* length-l) nil)
     ;; l is identical to some tail of *option-list*, i.e., l is
     ;; the max subset of its size
     ((= length-l (length (member (car l) *option-list*))) 
      (butlast *option-list* (- *length-of-option-list* length-l 1)))
     ;; there must be some elt in l whose immediate successor in
     ;; *option-list* is not in l.  Return the result of replacing
     ;; elt by its successor in l
	  (t 
	   (let* ((elt (find-if 
			#'(lambda (x) 
			    (not (member 
				  (cadr (member x *option-list*))
				  l)))
			l))
		  (next (cadr (member elt *option-list*))))
	     (substitute next elt l))))))









