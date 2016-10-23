;;; -*- Mode:LISP; Package:auto -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;



(in-package "AUTO")
(part-of ms88)

(context ms88)

(deffile timing
  (part-of ms88)
  (extension lisp)
  (mhelp "Timing stuff to the mating search package."))

;;;New facilities for time-record(hx 12/28/92)

(eval-when (compile load eval)
(defvar *lucid-gc-time* 0)
(defvar *lucid-gc-count* 0))

#+(and sbcl cmu new-compiler)
(eval-when (compile load eval)
(defun get-gc-run-time ()
  *gc-run-time*))

#+allegro
(eval-when (compile load eval)
(defun get-gc-run-time ()
  (let ((hxtimes (cddr (multiple-value-list (excl::get-internal-run-times)))))
        (+ (car hxtimes) (cadr hxtimes)))))
;;note excl::get-internal-run-times returns (A B C D) where A is user CPU time,
;;B is system CPU time, C is user GC time and D is system GC time
;;I'm not sure what the difference is between these two, but clearly
;;if we're going to count both, we should do so in both GC and run times.
;;Otherwise, the timing functions on GTPS tend to return negative search times!
;;Hence the new function tps-get-internal-run-time, below.
;;MB Tue Jun  2 13:50:21 1998

#+lucid
(eval-when (compile load eval)
(defun get-gc-run-time ()
  (let ((ret-val auto::*lucid-gc-time*))
    ret-val)))

#+lucid
(defun manual-lucid-garbage-counter (w)
  (if (eq w :before) (setq auto::*lucid-gc-count* (tps-get-internal-run-time))
    (if (eq w :after) 
	(progn (setq auto::*lucid-gc-time* 
		     (+ auto::*lucid-gc-time* (- (tps-get-internal-run-time) auto::*lucid-gc-count*)))
	       (setq auto::*lucid-gc-count* 0)))))

; this function is called by the lucid garbage-collection hook *gc-silence*
; both before and after GC, with values :before and :after 
; (*gc-silence* is initialised in tps3.ini, as below).
; Lucid also has "ephemeral garbage collection", which has no such hook,
; so we turn EGC off in the tps3.ini file (otherwise almost all GCs are EGCs
; and we can't time them).   (MB 12/94)

#+lucid
(setq *gc-silence* #'manual-lucid-garbage-counter)

#-(or (and cmu new-compiler) allegro lucid)
(progn 
   (eval-when (compile load eval)
   (warn "If internal-run-time contains garbage-collecting-time,
you need define a function to calculate garbage-collecting-time;
Otherwise, we do not count garbage-collecting-time."))
(defun get-gc-run-time () 0))

#+(or (and cmu new-compiler) allegro lucid)
(progn (if (boundp 'beginning-of-tps-gc)
           (makunbound 'beginning-of-tps-gc))
           (defconstnt beginning-of-tps-gc (get-gc-run-time)))


(defflag excluding-gc-time
  (flagtype boolean)
  (default nil)
  (subjects system mating-search)
  (mhelp "If T, we can use the function get-net-internal-run-time to exclude 
the gc time in recordings. Otherwise, get-net-internal-run-time is the
same as get-internal-run-time. The value of the flag should not be changed. 
This is a nominal flag, whose value does not affect the system at all except
telling users the message above. Check the flags SEARCH-TIME-LIMIT and
MAX-SEARCH-LIMIT to get more information."))

;;;Please set excluding-gc-time to T whenenve you find a way to calculate 
;;;gc time in some kind of LISP not listed in the following. Meanwile
;;;don't forget to write a version of funtion get-net-internal-run-time for
;;;that kind of LISP. 

(defflag timing-named
  (flagtype boolean)
  (default nil)
  (subjects system mating-search)
  (mhelp "If T, the labels printed by display-time will be shortened 
to allow room for the name of the current dproof, if there is one.
If NIL, then they won't.
Abbreviations used are: PRE - preprocessing, MS - mating search, 
U - unification, PPR - postprocessing, MRG - merging, 
TRA - translation, PRT - printing."))

#+(or (and cmu new-compiler) allegro lucid)
(setq excluding-gc-time T)

(defvar co-proof-mating-count -1)

;;;The following function is used to count the number of proofs
;;;to which several processes have been applied.

(defvar timing-hash-table (make-hash-table))

#+(or (and cmu new-compiler) allegro lucid)
(defconstnt timing-functions 
     '(get-gc-run-time tps-get-internal-run-time get-universal-time))

#-(or (and cmu new-compiler) allegro lucid)
(defconstnt timing-functions 
     '(tps-get-internal-run-time get-universal-time))

(defmacro timing-gethash (key1 key2)
   `(let* ((val1 (gethash ,key1 timing-hash-table))
           (val2 (assoc ,key2 val1)))
        (cdr val2)))

(defmacro timing-sethash (key1 key2 newval)
   `(let* ((val1 (gethash ,key1 timing-hash-table))
           (val2 (assoc ,key2 val1)))
       (rplacd val2 ,newval)))
          
(defun proof-count (name)
  (declare (special num-of-splitting))
  (let ((count (or (timing-gethash 'mating 'proof-count) -1)))
    (case name
         (prove (timing-sethash 'prove 'proof-count count))
	 (diy2 (timing-sethash 'diy2 'proof-count -1))
         (diy (timing-sethash 'diy 'proof-count
                  (if (< co-proof-mating-count count) count -1)))
         (mating (when (zerop num-of-splitting)
                       (timing-sethash 'mating 'proof-count (1+ count))
		       (timing-sethash 'diy 'proof-count 0)))
	 (unification (timing-sethash 'unification 'proof-count (or (timing-gethash 'unification 'proof-count) 0)))
;;;      (postprocess (timing-sethash 'postprocess 'proof-count count))
         (mating-ctr (when (not (zerop num-of-splitting))
                           (timing-sethash 'mating 'proof-count (1+ count))
		           (timing-sethash 'diy 'proof-count 0)))
         (merge (timing-sethash 'merge 'proof-count count))
         (eproof (timing-sethash 'eproof 'proof-count 
                       (timing-gethash 'merge 'proof-count)))
         (printproof (timing-sethash 'printproof 'proof-count 
                       (timing-gethash 'eproof 'proof-count)))
         (t (throwfail "Input is wrong.")))))

(defun timing-set-initial-values (name)
  (setf (gethash name timing-hash-table)
        (list (cons 'proof-count 0)
              (cons 'start nil) (cons 'last nil)
              (cons 'break nil) (cons 'runcount nil) (cons 'sofar nil))))

;;;---------------------------
;;;Notice the output of the following function is in the reverse order.
;;;compared with the list of timing-functions
(defun setclocks (name sort)
   (let (times)
      (dolist (fn timing-functions)
              (push (funcall fn) times))
      (timing-sethash name sort times)))

#+(or (and cmu new-compiler) allegro lucid)
(defmacro get-net-internal-run-time ()
  `(- (tps-get-internal-run-time) (- (get-gc-run-time) beginning-of-tps-gc)))

#-(or (and cmu new-compiler) allegro lucid)
(defmacro get-net-internal-run-time () `(tps-get-internal-run-time))

(defun time-conversion (seconds)
  (cond ((< seconds 60) "")
	((< seconds 3600) (format nil "(~3,1F mins)" (/ seconds 60)))
	((< seconds 86400) (format nil "(~3,1F hrs)" (/ seconds 3600)))
	(t (format nil "(~3,1F days)" (/ seconds 86400)))))

#+(or (and cmu new-compiler) allegro lucid)
(defun time-conversion-format (t1 t2 t3 t4 &optional intr)
  (let ((tc1 (time-conversion t1))
        (tc2 (time-conversion t2))
        (tc3 (time-conversion t3))
        (tc4 (time-conversion t4)))
   (case style
     ((xterm generic concept concept-s istyle) (format nil "~32@A ~18@A ~12@A ~12@A" tc1 tc2 tc3 tc4))
     (scribe (format nil "@\\@>~A@\\@>~A@\\@>~A@\\@>~A@\\" tc1 tc2 tc3 tc4))
     ((tex tex-1)  (if intr
                       (format nil "\\timeline{(Interrupted)}{~A}{~A}{~A}{~A}" tc1 tc2 tc3 tc4)
                       (format nil "\\timeline{}{~A}{~A}{~A}{~A}" tc1 tc2 tc3 tc4))))))


#-(or (and cmu new-compiler) allegro lucid)
(defun time-conversion-format (t1 t2 &optional intr)
  (let ((tc1 (time-conversion t1))
        (tc2 (time-conversion t2)))
    (case style
      ((xterm generic concept concept-s istyle) (format nil "~32@A  ~16@A" (time-conversion t1) (time-conversion t2)))
      (scribe (format nil "@\\@>~A@\\@>~A@\\" (time-conversion t1) (time-conversion t2)))
      ((tex tex-1) (if intr
                       (format nil "\\timeline{(Interrupted)}{~A}{~A}{}{}" tc1 tc2)
                       (format nil "\\timeline{}{~A}{~A}{}{}" tc1 tc2))))))


#+(or (and cmu new-compiler) allegro lucid)
(defun output-begining-time ()
  (case style
   ((xterm generic concept concept-s istyle)
    (msg (format nil "
The time used in each process:
-----------------------------------------------------------------------------~@
Process Name         | Realtime | Internal-runtime |  GC-time   | I-GC-time  ") t))
   (scribe
    (msg (format nil "
@begin(format)@tabset(1.8 inches, 2.9 inches, 4.4 inches, 5.25 inches)
The time used in each process:
@bar()
Process Name@\\@vbar@>Realtime @\\@vbar@>Internal-runtime @\\@vbar@>GC-time @\\@vbar@>I-GC-time") t))
   ((tex tex-1)
    (msg (format nil "~%~%\\vskip 0pt plus 100fill
\\vbox{\\hbox{Time used in each process:}
\\vskip6pt
\\hbox{\\vrule
\\vbox{
\\hrule
\\timeline{Process Name}{Realtime}{Internal-runtime}{GC-time}{I-GC-time}") t))))
;the following only works the first time around in each file...
;\\hbox{\\strut\\box\\processname\\vrule \\box\\realtime\\vrule 
;      \\box\\inruntime\\vrule \\box\\gctime\\vrule \\box\\igctime}") t))))

(defun name-conversion (name)
  (let ((*tag-times* (and dproof timing-named)))
    (case name
      (diy2 (if *tag-times*
		(subseq (concatenate 'string "DIY2 " (princ-to-string dproof) "                   ") 0 19)
	      "DIY2"))
      (diy (if *tag-times*
	       (subseq (concatenate 'string "DIY " (princ-to-string dproof) "                   ") 0 19)
	     "DIY"))
      (preprocess (if *tag-times*
		      (subseq (concatenate 'string "PRE " (princ-to-string dproof) "                   ") 0 19)
		    "Preprocessing"))
      (mating (if *tag-times*
		  (subseq (concatenate 'string "MS  " (princ-to-string dproof) "                   ") 0 19)
		"Mating Search"))
      (unification (if *tag-times*
		       (subseq (concatenate 'string "  U " (princ-to-string dproof) "                   ") 0 19)
		     "   (unification)"))
      (mating-ctr (if *tag-times*
		      (subseq (concatenate 'string "MS  " (princ-to-string dproof) "                   ") 0 19)
		    "Mating Search"))
      (merge (if *tag-times*
		 (subseq (concatenate 'string "MRG " (princ-to-string dproof) "                   ") 0 19)
	       "Merging Etree"))
      (postprocess (if *tag-times*
		       (subseq (concatenate 'string "PPR " (princ-to-string dproof) "                   ") 0 19)
		     "Postprocessing"))
      (eproof (if *tag-times*
		  (subseq (concatenate 'string "TRA " (princ-to-string dproof) "                   ") 0 19)
		"Proof Transformation"))
      (printproof (if *tag-times*
		      (subseq (concatenate 'string "PRT " (princ-to-string dproof) "                   ") 0 19)
		    "Printing Proof"))
      (t (if *tag-times*
	     (subseq (concatenate 'string "??? " (princ-to-string dproof) "                   ") 0 19)
	   "Anonymous")))))

(defun whether-called (name)
  (let ((diy2-count (timing-gethash 'diy2 'proof-count))
	(diy-count (timing-gethash 'diy 'proof-count))
        (mating-count (timing-gethash 'mating 'proof-count)))
    (case name
      (diy2 (and diy2-count (> diy2-count -1)))
      ((diy preprocess) (or (eq diy-count mating-count) (< diy-count 0)))
      ((mating mating-ctr) (> diy-count -1))
      (unification (or (> diy-count -1) (= diy-count -999)))
      (postprocess (and (> diy-count -1) (timing-gethash 'mating 'runcount)))
      (merge (and (> diy-count -1) 
                  (eq (timing-gethash 'merge 'proof-count) mating-count)))
      (eproof (and (> diy-count -1) 
                   (eq (timing-gethash 'eproof 'proof-count) mating-count)))
      (printproof (and (> diy-count -1)
                       (eq (timing-gethash 'printproof 'proof-count) mating-count))))))


#+(or (and cmu new-compiler) allegro lucid)
(defun output-format-time (name)
 (multiple-value-bind (vh vt) (diffcount name)
 (if (whether-called name)
  (let ((v1 (or (car vh) 0)) 
        (v2 (/ (or (cadr vh) 0) internal-time-units-per-second))
        (v3 (/ (or (caddr vh) 0) internal-time-units-per-second)))
   (case style
    ((xterm generic concept concept-s istyle)
     (if vt
      (progn (msg (format nil
"-----------------------------------------------------------------------------
                                (Interrupted)~@
~20A | ~8D | ~16,2F | ~10,2F | ~10,2F"
          (name-conversion name)
          v1 v2 v3 (- v2 v3)) t)
             (when (or (< 60 v1) (< 60 v2)) (msg (time-conversion-format v1 v2 v3 (- v2 v3)) t)))
      (progn (msg (format nil
"-----------------------------------------------------------------------------~@
~20A | ~8D | ~16,2F | ~10,2F | ~10,2F"
          (name-conversion name)
          v1 v2 v3 (- v2 v3)) t)
             (when (or (< 60 v1) (< 60 v2)) (msg (time-conversion-format v1 v2 v3 (- v2 v3)) t)))))
    (scribe
     (if vt
	 (progn (msg (format nil
"@bar()
@\\@\\@>(Interrupted)@\\~@
~A @\\@vbar@>~D @\\@vbar@>~10,2F @\\@vbar@>~8,2F @\\@vbar@>~10,2F"
          (name-conversion name)
          v1 v2 v3 (- v2 v3)) t)
             (when (or (< 60 v1) (< 60 v2)) (msg (time-conversion-format v1 v2 v3 (- v2 v3)) t)))
      (progn (msg (format nil
"@bar()
~A @\\@vbar@>~D @\\@vbar@>~10,2F @\\@vbar@>~8,2F @\\@vbar@>~10,2F"
          (name-conversion name)
          v1 v2 v3 (- v2 v3)) t)
             (when (or (< 60 v1) (< 60 v2)) (msg (time-conversion-format v1 v2 v3 (- v2 v3)) t)))))
     ((tex tex-1)
       (if vt
      (progn (msg (format nil "\\hrule
\\timeline{~A}{~D}{~,2F}{~,2F}{~,2F}"
          (name-conversion name)
          v1 v2 v3 (- v2 v3)) t)
             (msg (time-conversion-format v1 v2 v3 (- v2 v3) t) t))
      (progn (msg (format nil"\\hrule
\\timeline{~A}{~D}{~,2F}{~,2F}{~,2F}"
          (name-conversion name)
          v1 v2 v3 (- v2 v3)) t)
             (when (or (< 60 v1) (< 60 v2)) (msg (time-conversion-format v1 v2 v3 (- v2 v3)) t)))))))
      (case style
       ((xterm generic concept concept-s istyle) 
        (msg (format nil
"-----------------------------------------------------------------------------~@
~20A | not called in this proof"
           (name-conversion name)) t))
       (scribe 
        (msg (format nil
"@bar()
~A @\\@vbar not called in this proof"
           (name-conversion name)) t))
       ((tex tex-1) (msg (format nil "\\hrule
\\ntimeline{~A}"
           (name-conversion name)) t))))))




#+(or (and cmu new-compiler) allegro lucid)
(defun output-end-time ()
  (case style
   ((xterm generic concept concept-s istyle)
    (msg (format nil 
"-----------------------------------------------------------------------------~%")))
   (scribe
    (msg (format nil 
"@bar()
@end(format)")))
   ((tex tex-1)
    (msg (format nil "\\hrule}\\vrule}}") t))))


#-(or (and cmu new-compiler) allegro lucid)
(defun output-begining-time ()
  (case style
   ((xterm generic concept concept-s istyle)
    (msg (format nil "
The time used in each process:
---------------------------------------------------
Process Name         | Realtime | Internal-runtime ") t))
   (scribe
    (msg (format nil "
@begin(format)@tabdivide(3)
The time used in each process:
@bar
Process Name @\\@vbar@> Realtime @\\@vbar@> Internal-runtime ") t))
   ((tex tex-1) 
    (msg (format nil "~%~%\\vskip 0pt plus 100fill
\\vbox{\\hbox{Time used in each process:}
\\vskip6pt
\\hbox{\\vrule
\\vbox{
\\hrule
\\timeline{Process Name}{Realtime}{Internal-runtime}{GC-time}{I-GC-time}") t))))
;the following only works the first time around in each file...
;\\hbox{\\strut\\box\\processname\\vrule \\box\\realtime\\vrule 
;      \\box\\inruntime\\vrule \\box\\gctime\\vrule \\box\\igctime}") t))))

#-(or (and cmu new-compiler) allegro lucid)
(defun output-format-time (name)
 (multiple-value-bind (vh vt) (diffcount name)
  (if (whether-called name)
   (let ((v1 (or (car vh) 0)) (v2 (/ (or (cadr vh) 0) internal-time-units-per-second)))
     (case style
       ((xterm generic concept concept-s istyle)
         (if vt (progn (msg (format nil
"---------------------------------------------------
                   (Interrupted)~@
~20A | ~8D | ~16,2F "
          (name-conversion name)
          v1 v2) t)
             (when (or (< 60 v1) (< 60 v2)) (msg (time-conversion-format v1 v2) t)))
      (progn (msg (format nil
"---------------------------------------------------~@
~20A | ~8D | ~16,2F "
          (name-conversion name)
          v1 v2) t)
             (when (or (< 60 v1) (< 60 v2)) (msg (time-conversion-format v1 v2) t)))))
       (scribe 
         (if vt (progn (msg (format nil
"@bar()
@\\@\\@>(Interrupted)@\\~@
~A @\\@vbar@> ~D @\\@vbar@> ~10,2F "
          (name-conversion name)
          v1 v2) t)
             (when (or (< 60 v1) (< 60 v2)) (msg (time-conversion-format v1 v2) t)))
      (progn (msg (format nil
"@bar()
~A @\\@vbar@> ~D @\\@vbar@> ~10,2F "
          (name-conversion name)
          v1 v2) t)
             (when (or (< 60 v1) (< 60 v2)) (msg (time-conversion-format v1 v2) t)))))
       ((tex tex-1) (if vt 
      (progn (msg (format nil "\\hrule
\\timeline{~A}{~D}{~,2F}{N/A}{N/A}"
          (name-conversion name)
          v1 v2) t)
             (msg (time-conversion-format v1 v2 t) t))
      (progn (msg (format nil"\\hrule
\\timeline{~A}{~D}{~,2F}{N/A}{N/A}"
          (name-conversion name)
          v1 v2) t)
             (when (or (< 60 v1) (< 60 v2)) (msg (time-conversion-format v1 v2) t)))))))
     (case style
       ((xterm generic concept concept-s istyle) 
      (msg (format nil
"---------------------------------------------------~@
~20A | not called in this proof"
          (name-conversion name)) t))
       (scribe
      (msg (format nil
"@bar()
~A @\\@vbar not called in this proof"
          (name-conversion name)) t))
       ((tex tex-1) (msg (format nil "\\hrule
\\ntimeline{~A}"
           (name-conversion name)) t))))))




#-(or (and cmu new-compiler) allegro lucid)
(defun output-end-time ()
  (case style
   ((xterm generic concept concept-s istyle)
    (msg (format nil 
"---------------------------------------------------~%")))
   (scribe
    (msg (format nil 
"@bar()
@end(format)")))
   ((tex tex-1) 
    (msg (format nil "\\hrule}\\vrule}}") t))))


(defun diffcount (name)
   (let ((hxbreak (not (timing-gethash name 'runcount)))
         (hxstart (timing-gethash name 'start))
         hxlast)
      (cond ((eq name 'mating-ctr) 
             (setq hxbreak (not (timing-gethash 'mating 'runcount)))
             (if hxbreak
                 (setq  hxlast (timing-gethash 'mating-ctr 'break))
                 (setq  hxlast (timing-gethash 'mating 'last))))  
	    ((eq name 'unification)
             (setq hxbreak (not (timing-gethash 'unification 'runcount)))
	     (setq hxlast (timing-gethash 'unification 'sofar)))
            ((eq name 'postprocess)
             (setq hxbreak (not (timing-gethash 'mating-ctr 'runcount)))
             (if hxbreak 
                 (setq  hxlast (timing-gethash 'mating-ctr 'break))
                 (setq  hxlast (timing-gethash 'mating-ctr 'last))))                  
            (hxbreak (setq hxlast (timing-gethash name 'break)))
            (t (setq hxlast (timing-gethash name 'last))))
      (values (mapcar #'- hxlast hxstart) hxbreak)))

#+(or (and cmu new-compiler) allegro lucid)
(defmacro diffcount-internal-net (name)
   (let ((timelist (gensym)))
    `(let* ((,timelist (cdr (diffcount ,name))))
       (- (car ,timelist) (cadr ,timelist)))))
        
#-(or (and cmu new-compiler) allegro lucid)
(defmacro diffcount-internal-net (name)
   `(cadr (diffcount ,name)))

(defun display-time (name)
  (declare (special num-of-splitting))
  (if (and (zerop num-of-splitting) (eq co-proof-mating-count -1) 
           (zerop (timing-gethash 'mating 'proof-count))
	   (not (and (eq name 'unification) (eq (timing-gethash 'diy 'proof-count) -999))))
      (msg t "No automatic proof has been started." t "Timing record is not available." t)
      (progn (output-begining-time)
             (if (eq name 'all)
                 (mapcar #'output-format-time 
			 (if (whether-called 'diy2)
			     '(diy2 diy preprocess mating-ctr unification postprocess merge eproof printproof)
			   '(diy preprocess mating-ctr unification postprocess merge eproof printproof)))
                 (if (and (eq name 'mating) (timing-gethash 'unification 'sofar))
		     (mapcar #'output-format-time '(mating unification))
		   (output-format-time name)))
             (output-end-time)
             (if (eq name 'all) (msg "Remarks: time unit is second; mating search time includes unification." t) (msg t)))))

(defun display-time-in-daterec ()
   (msg t)
   (if (< (timing-gethash 'prove 'proof-count) 
          (timing-gethash 'mating 'proof-count))
       (progn (output-begining-time)
              (mapcar #'output-format-time 
		      (if (whether-called 'diy2)
			  '(diy2 diy preprocess mating-ctr unification postprocess merge eproof printproof)
			'(diy preprocess mating-ctr unification postprocess merge eproof printproof)))
              (output-end-time))
       (msg "The proof was not finished automatically." t)))

;;;The above are new facilities for time-record.(hx 4/9/92)
;;;*******************************

;;;The following is a list of timed processes
(mapcar #'timing-set-initial-values  
  '(prove diy2 diy preprocess mating-ctr mating unification postprocess merge eproof printproof))

(defvar unif-running nil)

(defun breakcount (name)
    (proof-count name)
    (setclocks name 'break)
;    (when (eq name 'mating)
;        (timing-sethash 'postprocess 'break 
;                        (timing-gethash 'mating 'break)))
    (when (eq name 'unification)
      (setq unif-running nil) ;(msg ")")
      (timing-sethash 'unification 'sofar (mapcar #'+ 
						  (timing-gethash 'unification 'sofar)
						  (mapcar #'(lambda (x y) (- x y)) 
							  (timing-gethash 'unification 'break)
							  (timing-gethash 'unification 'last)))))
    (when (eq name 'diy) 
        (timing-sethash 'preprocess 'break 
                        (timing-gethash 'diy 'break))
        (timing-sethash 'printproof 'break 
                        (timing-gethash 'diy 'break))))

(defun runcount (name)
    (timing-sethash name 'runcount T)
    (when (eq name 'unification)
      (setq unif-running t) ;(msg "(")
      (when (not (timing-gethash 'unification 'sofar))
	(startcount 'unification)
	(setclocks 'unification 'sofar)))
    (setclocks name 'last)
    (when (eq name 'mating) 
          (timing-sethash 'postprocess 'start
                (timing-gethash 'mating 'last)))
    (when (eq name 'diy) 
        (timing-sethash 'printproof 'runcount T)
        (timing-sethash 'printproof 'last
                        (timing-gethash 'diy 'last))))

(defun startcount (name)
    (timing-sethash name 'runcount nil) 
    (setclocks name 'start)
    (when (eq name 'mating-ctr)
	  (timing-sethash 'unification 'sofar nil)
          (timing-sethash 'mating 'runcount nil)
          (timing-sethash 'mating 'start
                          (timing-gethash 'mating-ctr 'start)))
    (when (eq name 'diy) 
          (timing-sethash 'preprocess 'runcount nil)
          (timing-sethash 'preprocess 'start
                          (timing-gethash 'diy 'start))))


;;;*******************************



