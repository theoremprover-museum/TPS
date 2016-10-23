;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of METAWFFS)

;;;
;;; File: META-VAR2
;;; Package: Metawffs
;;;
;;; defines the META-VAR flavor for labels and some functions on them.

(part-of metawffs)

(deffile meta-var2
  (part-of metawffs)
  (extension lsp)
  (mhelp "Further functions on labels."))

(context prim-obj)

(defun make-l-wffschema (gwff)
  (let ((wffconstlist nil) (propsymalist nil))
    (declare (special wffconstlist propsymalist))
    (make-l-wffschema1 gwff nil)))

(defun make-l-wffschema1 (gwff bdvars)
  (declare (special wffconstlist propsymalist))
  (cond ((label-q gwff) (apply-label gwff (make-l-wffschema1 gwff bdvars)))
	((lsymbol-q gwff)
	 (cond ((logconst-q gwff) gwff)
	       ((and (propsym-q gwff)
		     (not (member gwff bdvars)))
		;(format t "~&generating mv for ~A" gwff)
		(generate-meta-var gwff))
	       (t gwff)))
	((boundwff-q gwff)
	 (make-l-bdwffschema gwff (cons (bdvar gwff) bdvars)))
	(t (cons (make-l-wffschema1 (car gwff) bdvars)
		 (make-l-wffschema1 (cdr gwff) bdvars)))))

(defun make-l-meta-label-wffschema (gwff)
  (declare (special propsymalist))
  (let* ((new-meta-label (rename-wffop-label gwff))
	 (meta-args (get new-meta-label 'meta-args))
	 (new-arg-name (get new-meta-label 'meta-wffop)))
    ;; meta-labels are append at the very end!  Luckily, there aren't that
    ;; many.  It's also important they come AFTER the args are processed,
    ;; so that labels inside come before labels further outside.
    (putprop new-meta-label
	     (mapcar #'make-l-wffschema-if-wff meta-args)
	     'meta-args)
    (do ()
	((not (assoc new-arg-name propsymalist)))
      (setq new-arg-name (prepend new-arg-name '*)))
    (setq propsymalist
	  (append propsymalist (list (cons new-arg-name new-meta-label))))
    new-meta-label))

(defun make-l-wffschema-if-wff (wffop-arg)
  (if (and (symbolp wffop-arg) (get wffop-arg 'lisp-token))
      wffop-arg
      (make-l-wffschema1 wffop-arg nil)))

(defun make-l-bdwffschema (bdwff bdvars)
  (declare (special meta-bdvar-name))
  (let ((new-bdvar-label (create-name meta-bdvar-name)))
    (putprop new-bdvar-label 'meta-bd 'flavor)
    (putprop new-bdvar-label 
	     (acons (bdvar bdwff) (cdar bdwff)
		    (make-l-wffschema1 (cdr bdwff) bdvars))
	     'meta-wff)
    ;; Here we could remove the last pair binding (caar bdwff) from
    ;; propsymalist, if we wanted to allow wffs which are not ab-normal.
    new-bdvar-label))

(defun make-m-wffschema (gwff)
  (let ((wffconstlist nil) (propsymalist nil))
    (declare (special wffconstlist propsymalist))
    (make-m-wffschema1 gwff)))

(defun make-m-wffschema1 (gwff)
  (declare (special wffconstlist propsymalist))
  (cond ((label-q gwff) (apply-label gwff (make-m-wffschema1 gwff)))
	((lsymbol-q gwff)
	 (cond ((logconst-q gwff) gwff)
	       ((propsym-q gwff)
		(generate-meta-var gwff))
	       (t gwff)))
	((boundwff-q gwff)
	 (make-m-bdwffschema gwff))
	(t (cons (make-m-wffschema1 (car gwff))
		 (make-m-wffschema1 (cdr gwff))))))

(defun make-m-meta-label-wffschema (gwff)
  (declare (special propsymalist))
  (let* ((new-meta-label (rename-wffop-label gwff))
	 (meta-args (get new-meta-label 'meta-args))
	 (new-arg-name (get new-meta-label 'meta-wffop)))
    ;; meta-labels are append at the very end!  Luckily, there aren't that
    ;; many.  It's also important they come AFTER the args are processed,
    ;; so that labels inside come before labels further outside.
    (putprop new-meta-label
	     (mapcar #'make-m-wffschema-if-wff meta-args)
	     'meta-args)
    (do ()
	((not (assoc new-arg-name propsymalist)))
      (setq new-arg-name (prepend new-arg-name '*)))
    (setq propsymalist
	  (append propsymalist (list (cons new-arg-name new-meta-label))))
    new-meta-label))

(defun make-m-wffschema-if-wff (wffop-arg)
  (if (and (symbolp wffop-arg) (get wffop-arg 'lisp-token))
      wffop-arg
      (make-m-wffschema1 wffop-arg)))

(defun make-m-bdwffschema (bdwff)
  (declare (special meta-bdvar-name propsymalist)) ; mkaminski 11/15/2005
  (let ((propsymalist (remove-if #'(lambda (x) (eq x (bdvar bdwff)))
				 propsymalist)))
    (declare (special propsymalist))
    (let ((new-bdvar-label (create-name meta-bdvar-name))
	  (mv (generate-meta-var (caar bdwff))))
      (putprop new-bdvar-label 'meta-bd 'flavor)
      (putprop new-bdvar-label 
	       (cons (cons mv (cdar bdwff))
		     (make-m-wffschema1 (cdr bdwff)))
	       'meta-wff)
    ;; Here we could remove the last pair binding (caar bdwff) from
    ;; propsymalist, if we wanted to allow wffs which are not ab-normal.
      new-bdvar-label)))
