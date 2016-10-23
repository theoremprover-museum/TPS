;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of TPS-HELP)

;;;
;;; File: Read-Help
;;;
;;; looks through a list of file to find help for a keyword.
;;;

(part-of tps-help)

(deffile read-help
  (part-of tps-help)
  (extension lsp)
  (mhelp "Looks through a list of packages and writes the help-string in
them into file(s), sorted alphabetically."))



(context rd-help)

;;This needs to be modifed.

;*;(defun read-help (key filename)
;*;  (let ((full-filename (let ((defaultf `(,source-dir * help)))
;*;			 (probef filename))))
;*;    (if (not full-filename) (throwfail "Help file " filename " not found")
;*;	(let ((helpf (open full-filename '(in ascii))))
;*;	  (unwind-protect
;*;	   (do ((char (tyi helpf '-1) (tyi helpf '-1))
;*;		(foundp nil)
;*;		(found-key))
;*;	       ((= char '-1) nil)
;*;	     (when (and (= char #\return) (tyi helpf) ; gobble <lf>
;*;			(= (setq char (tyi helpf -1)) #\return) (tyi helpf)
;*;			(= (setq char (tyi helpf -1)) #/())
;*;		   (setq found-key (read helpf))
;*;		   (cond ((eq found-key key)
;*;			  (setq foundp t)
;*;			  (push (cons (read helpf) (read helpf))
;*;				(get key 'mhelp)))
;*;			 (foundp (return t)))))
;*;	   (close helpf))))))

(defun read-help (key filename) 
  (declare (ignore key filename)) nil)


