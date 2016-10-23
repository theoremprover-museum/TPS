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
;;; File: META-VAR
;;; Package: Metawffs
;;;
;;; defines the META-VAR flavor for labels and some functions on them.

(part-of metawffs)

(deffile meta-var
  (part-of metawffs)
  (extension lsp)
  (mhelp "Defines the META-VAR flavor for labels and some functions on them."))

(context prim-obj)

(defflag meta-var-name
  (flagtype symbol)
  (subjects internal-names)
  (default mv)
  (mhelp "The prefix for names of meta variables."))

(defflag meta-bdvar-name
  (flagtype symbol)
  (subjects internal-names)
  (default bd)
  (mhelp "The prefix for names of bound meta variables."))

(defflavor meta-var
  (mhelp "A label which stands for a meta-variable.")
  ;; For matching and substituting into wffs.
  (match-bind meta-var-match-bind)
  (meta-subst1 meta-subst-meta-var)
  ;; Printing Wffops
  (printwff (lambda (wff brackets depth)
	      (declare (ignore brackets depth))
	      (if ppvirtflag (throwfail "Cannot pretty-print META-VAR.")
		  (progn (pp-symbol-space '\!)
			 (pp-lsymbol (get wff 'var-name))))))
  (prt-symbol-p (lambda (gwff)(declare (ignore gwff)) t))
  (prt-infix-op (lambda (gwff)(declare (ignore gwff)) nil))
  (prt-prefix-op (lambda (gwff)(declare (ignore gwff)) nil))
  (prt-associative-p (lambda (gwff)(declare (ignore gwff)) nil))
  ;; Misc required operations
  (type (lambda (gwff) (get gwff 'meta-type)))
  (gwff-p (lambda (gwff) (declare (ignore gwff))t))
  (gwff-q (lambda (gwff)(declare (ignore gwff)) t))
  (legal-type-p1 (lambda (gwff) (get gwff 'meta-type)))
  ;; default for test operations test the symbol it stand for
  (default-boolean-result (lambda (gwff) (get gwff 'var-name)))
  )

(defflavor meta-bd
  (mhelp "A label created when a bound meta-variable appears.")
  ;; For matching and substituting
  (match-bind meta-bdvar-match-bind)
  (meta-subst1 meta-subst-meta-bd)
  ;; Printing Wffops
  (printwff (lambda (wff brackets depth)
	      (printwff (get wff 'meta-wff) brackets depth)))
  (prt-symbol-p (lambda (gwff)(declare (ignore gwff)) nil))
  (prt-infix-op (lambda (gwff)(declare (ignore gwff)) nil))
  (prt-prefix-op (lambda (gwff)(declare (ignore gwff)) nil))
  (prt-associative-p (lambda (gwff)(declare (ignore gwff)) nil))
  ;; Misc required operations
  (type (lambda (gwff) (boundwfftype (get gwff 'meta-wff))))
  (gwff-p (lambda (gwff) (gwff-p (get gwff 'meta-wff))))
  (gwff-q (lambda (gwff) (declare (ignore gwff))t))
  (legal-type-p1 (%catch% (boundwfftype (get gwff 'meta-wff))
			(fail nil)))
  ;; default for test operations
  (default-boolean-result (lambda (gwff) (get gwff 'meta-wff)))
  )

(defwffop make-wffschema
  (argtypes gwff)
  (resulttype gwff)
  (argnames gwff)
  (arghelp "The wff to make into a wffschema.")
  (applicable-p ab-normal-p)
  (mhelp "Translate a gwff into a wffschema by replacing proper symbols
by labels of type META-VAR."))


(defun make-wffschema (gwff)
  (let ((wffconstlist nil) (propsymalist nil))
    (declare (special wffconstlist propsymalist))
    (make-wffschema1 gwff)))


(defun meta-ize-gwff (gwff) (make-wffschema1 gwff))


(defwffrec make-wffschema1 (argnames gwff))

(defun make-wffschema1 (gwff)
  (declare (special wffconstlist propsymalist))
  (cond ((label-q gwff) (apply-label gwff (make-wffschema1 gwff)))
	((lsymbol-q gwff)
	 (cond ((logconst-q gwff) gwff)
	       ((propsym-q gwff)
		(generate-meta-var gwff))
	       (t gwff)))
	((boundwff-q gwff)
	 (make-bdwffschema gwff))
	(t (cons (make-wffschema1 (car gwff))
		 (make-wffschema1 (cdr gwff))))))

(defun make-meta-label-wffschema (gwff)
  (declare (special propsymalist))
  (let* ((new-meta-label (rename-wffop-label gwff))
	 (meta-args (get new-meta-label 'meta-args))
	 (new-arg-name (get new-meta-label 'meta-wffop)))
    ;; meta-labels are append at the very end!  Luckily, there aren't that
    ;; many.  It's also important they come AFTER the args are processed,
    ;; so that labels inside come before labels further outside.
    (putprop new-meta-label
	     (mapcar #'make-wffschema-if-wff meta-args)
	     'meta-args)
    (do ()
	((not (assoc new-arg-name propsymalist)))
      (setq new-arg-name (prepend new-arg-name '*)))
    (setq propsymalist
	  (append propsymalist (list (cons new-arg-name new-meta-label))))
    new-meta-label))

(defun make-wffschema-if-wff (wffop-arg)
  (if (and (symbolp wffop-arg) (get wffop-arg 'lisp-token))
      wffop-arg
      (make-wffschema1 wffop-arg)))

(defun make-bdwffschema (bdwff)
  (declare (special meta-bdvar-name)); propsymalist)) ; mkaminski 11/15/2005
  ;(let ((propsymalist (remove-if #'(lambda (x) (eq x (bdvar bdwff)))
	;			 propsymalist)))
    ;(declare (special propsymalist))
  (let ((new-bdvar-label (create-name meta-bdvar-name))
	(mv (generate-meta-var (caar bdwff))))
    (putprop new-bdvar-label 'meta-bd 'flavor)
    (putprop new-bdvar-label 
	     (cons (cons mv (cdar bdwff))
		   (make-wffschema1 (cdr bdwff)))
	     'meta-wff)
    ;; Here we could remove the last pair binding (caar bdwff) from
    ;; propsymalist, if we wanted to allow wffs which are not ab-normal.
    new-bdvar-label));)

(defun create-meta-var-name (propsym)
  (declare (special meta-var-name propsymalist))
  (let ((mv (create-name meta-var-name)))
    (putprop mv 'meta-var 'flavor)
    (putprop mv propsym 'var-name)
    (putprop mv (get propsym 'type) 'meta-type)
    (push (cons propsym mv) propsymalist)
    mv))

(defun generate-meta-var (propsym)
  (declare (special propsymalist))
  (let ((occ (assoc propsym propsymalist)))
    (if (not occ)
	(create-meta-var-name propsym)
	(cdr occ))))
