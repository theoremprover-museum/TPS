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
;;; File: OTL-FILEOUT

(part-of otlnl)

(deffile otl-fileout
  (part-of otlnl)
  (extension lsp)
  (mhelp "Contains functions which allow writing into files inside the
outline package."))

(context otl-files)


(defmexpr printproof
  (argtypes filespec)
  (argnames filename)
  (arghelp "Filename")
  (defaultfns (lambda (filename)
		(list
		  (if (eq filename '$)
		      (namestring
			(make-pathname%
			  :name (string-downcase (symbol-name dproof))
			  :type "prt"))
		    filename))))
  (print-command t)
  (mhelp "Print the current proof into a file."))

(defun printproof (filename)
  ;;(let ((err-p)))
  (reroute-output filename
		  (namestring (make-pathname%
				:name (string-downcase (symbol-name dproof))
				:type "prt"))
		  (format T "Proof of exercise ~A printed for ~A on ~A~%"
			  dproof (status-userid) (stringdt nil))
		  (let ((style 'generic))
		    ;;(setq err-p (null (errset (pall))))
		    (pall)
		    (when (and print-comments (get dproof 'comment)) (msg t) (eval (get dproof 'comment)) (msg t))))
  ;;(cond (err-p (tps-warning t "Proof may not be completely printed!")))
  )

;;; The effect of WAIT is now achieved by a SIGNAL-EVENT in a loop
;;; as in the example of REMARK
;*;(defun wait (filename)
;*;  (cond ((not (probef filename))
;*;	 (Throwfail "The file which contains ETPS records does not exist.  "
;*;		    "Please inform your instructor.")))
;*;  (do ((file-obj (car (errset (open filename '(append ascii)) nil)) 
;*;		 (car (errset (open filename '(append ascii)) nil))))
;*;      ((filep file-obj) file-obj)
;*;    (msg "Could not open file.  Please wait... (CTRL-G to abort)")
;*;    (terpri)
;*;    (sleep 0.5)))

