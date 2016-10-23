;;; -*- Mode:LISP; Package:auto -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1990 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;


(in-package :AUTO)
(part-of ms90-3)

(deffile ms90-3-path-bkup
  (part-of ms90-3)
  (mhelp "Functions for locating an earlier path when backtracking in
Path-focused duplication.")) 

(defun find-alt-conn (lit1 lit2 ind2 path open-path unif-prob env-stack
			   dup-record mating &optional (start-time nil) (time-limit nil))
  (multiple-value-bind (literal2 index2 unif-prob env2 subpath lit2)
      (find-alt-conn-rec lit2 ind2 lit1 env-stack path env-stack unif-prob start-time time-limit)
    (if literal2
	(let ((conn (make-conn :lit1 lit1 :lit2 literal2 :path path
                               :subpath (cdr subpath) :old-env env-stack :env env2)))
	  (values t unif-prob 
                  (cons conn dup-record) 
                  (cons conn (acons (cdar env-stack) index2 mating))))
	(multiple-value-bind (literal2 index2 unif-prob env2 subpath)
	    (find-alt-conn-rec lit2 ind2 lit1 env-stack open-path env2
			       unif-prob start-time time-limit)
	  (if literal2
	      (let ((conn (make-conn :lit1 lit1 :lit2 literal2 :path path
                                     :subpath (cdr subpath) :old-env env-stack :env env2)))
		(values t unif-prob 
                        (cons conn dup-record) 
                        (cons conn (acons (cdar env-stack) index2 mating))))
	      (values nil unif-prob dup-record mating))))))


(defun find-alt-conn-rec (lit2 ind2 lit1 env1 path env2 unif-prob &optional (start-time nil) (time-limit nil))
  (dolist (literal2 path (values nil nil unif-prob env2 nil lit2))
    (if (consp literal2) (setq env2 literal2)
	(unless (universal-p literal2) 
	    (if lit2 (if (and (eq lit2 literal2) (= (cdar env2) ind2))
                         (setq lit2 nil))
                (when (and (> (find-paths-below literal2) 0) (> (find-paths-below lit1) 0))
		      ;the second half of the above is a fix for max-mates Mon Aug 19 13:26:51 1996
                  (multiple-value-bind (new-unif-prob conn)
                      (conn-unif-p lit1 env1 literal2 env2 unif-prob start-time time-limit)
                    (when new-unif-prob
                      (when (eq mating-verbose 'max)
                        (msg t "Adding connection: " lit1 " . " literal2 4
                             (cdar env1) " . " (cdar env2)))
                      (decrease-paths-below literal2)
                      (decrease-paths-below lit1)
                      (return (values literal2 (cdar env2) new-unif-prob 
                                      conn (member literal2 path)))))))))))
