;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFFS)

;;;
;;; File: FLAVORING
;;; Package: WFFS
;;;
;;; defines the necessary macros and functions dealing with flavors
;;; of labels.
;;;
;;; Modified with new hash-table implementation of flavors 9/26/87 DAN

(deffile flavoring
  (part-of wffs)
  (extension clisp)
  (mhelp "Contains macros and functions for flavors of labels."))


(defstruct flavor
  (wffop-hash-table (make-hash-table :size 25 :test #'eq))
  constructor-fun
  name
  mhelp)

;;; We know that label is definitely some kind of label
;;; Major assumption: that type-of will work on structures that
;;; are being used to implement labels

#+comment(defmacro apply-label (label call-form)
  (let ((wffop (car call-form))
	(args (cdr call-form))
	(vvar (gensym)))
    `(let ((,vvar ,label))
       (funcall
	(let ((hash-table
	       (flavor-wffop-hash-table
		(gethash
		 (if (symbolp ,vvar)
		     (get ,vvar 'flavor)
		     (type-of ,vvar))
		 *flavor-hash-table*))))
	  (cond ((gethash ',wffop hash-table))
		((eq (get ',wffop 'resulttype) 'boolean)
		 (cond ((gethash 'default-boolean-result
				 hash-table))
		       (t (apply-label-error ,vvar ',wffop))))
		(t (apply-label-error ,vvar  ',wffop))))
	,@(if (get wffop 'multiple-recursion)
	      (cons vvar args)
	      args)))))

;;; This fixes the above, so that if we use the default-boolean-result, it
;;; is applied to the label, and the wffop recurses on the result.
;;; DAN 15APR91

(defmacro apply-label (label call-form)
  (let ((wffop (car call-form))
	(args (cdr call-form))
	(vvar (gensym)))
    `(let* ((,vvar ,label)
	    (hash-table
	     (flavor-wffop-hash-table
	      (gethash
	       (if (symbolp ,vvar)
		   (get ,vvar 'flavor)
		 (type-of ,vvar))
	       *flavor-hash-table*)))
	    (fn (gethash ',wffop hash-table)))
       (if fn 
	   (funcall fn ,@(if (get wffop 'multiple-recursion)
			     (cons vvar args)
			   args))
	 (if (and (eq (get ',wffop 'resulttype) 'boolean)
		  (setq fn (gethash 'default-boolean-result
				    hash-table)))
	     (funcall #',wffop 
		      ,@(substitute `(funcall fn ,label) `,label 
				    (if (get wffop 'multiple-recursion)
					(cons vvar args)
				      args)))
	   (apply-label-error ,vvar ',wffop))))))


(defun apply-label-error (label wffop)
  (throwfail "Wff Operation " wffop
	     " cannot be applied to labels of flavor "
	     (if (symbolp label) (get label 'flavor) (type-of label)) ".  "))

(eval-when (compile load eval)
  (putprop 'default-boolean-result '(gwff) 'argnames))

(context flavor-obj)

(defstruct structured-tps-label "Included in every structured TPS flavor type.
Thus we can easily test a wff to see if it is a structured flavor." 
bogus-slot)

(defvar *flavor-hash-table*
  (make-hash-table :size 30 :test #'eq))

(defcategory flavor
  (define defneverused) ;see macro defflavor below; we keep this because we want the category...
  (properties
   (inherit-properties multiple)
   (instance-attributes multiple)
   (include single)
   (printfn singlefn)
   (mhelp single))
  (other-prop read-flavor-fn)
  (global-list global-flavorlist)
  (mhelp-line "flavor")
  (mhelp-fn princ-mhelp))

(defun defflavorstruct (flavor-name include printfn slots)
  (let ((head (remove-if #'null (list flavor-name
				      (if include
					  (cons :include include)
					'(:include structured-tps-label))
				      (if printfn
					  (list :print-function printfn))))))
    `(defstruct ,head
       ,@slots)))


;;; Redefined 14FEB89 so that functions defined for the properties will
;;; be compiled.  DAN
(defmacro defflavor (flavor-name &rest props)
  (let* ((inherit-from (cdr (assoc 'inherit-properties props)))
	(mhelp-line (cadr (assoc 'mhelp props)))
	(include-struct (cdr (assoc 'include props)))
	(is-structured (if (or include-struct (cadr (assoc 'structured props)))
			   t))
	(attributes (cdr (assoc 'instance-attributes props)))
	(other-props
	 (remove-if #'(lambda (x)
			(memq (car x)
			      '(inherit-properties mhelp
				instance-attributes structured include)))
		    props))
	(defun-list nil)
	(hash-list nil))
    (do* ((other-props other-props (cdr other-props))
          (prop (car other-props) (car other-props))
          (other-prop-fn-name 
           (if prop (conc-names flavor-name "--" (car prop)))
           (if prop (conc-names flavor-name "--" (car prop))))
          (other-prop-fn 
           (if prop (read-flavor-fn (car prop) (cdr prop)
                                    other-prop-fn-name))
           (if prop (read-flavor-fn (car prop) (cdr prop)
                                    other-prop-fn-name))))
        ((null prop) defun-list)
      (unless (or (compiled-function-p other-prop-fn)
                  (symbolp other-prop-fn))
        (push other-prop-fn defun-list))
      (push (cons (car prop) 
                  (if (or (compiled-function-p other-prop-fn)
                          (symbolp other-prop-fn))
                      other-prop-fn
                      other-prop-fn-name))
            hash-list))
    (if is-structured
        (push (defflavorstruct flavor-name
                include-struct
                (or (cdr (assoc 'printfn hash-list))
		    (if include-struct
                        (gethash 'printfn (flavor-wffop-hash-table
                                           (gethash (car include-struct)
                                                    *flavor-hash-table*)))))
                attributes)
              defun-list))
    `(progn
       (let* ((flavor-struct
               (or (gethash ',flavor-name *flavor-hash-table*)
                   (make-flavor :name ',flavor-name)))
              (hash (flavor-wffop-hash-table flavor-struct))) 
         (pushnew ',flavor-name global-flavorlist)
         (pushnew (cons 'flavor current-context) (get ',flavor-name 'contexts))
         (setf (get ',flavor-name 'flavor) t)
         (when ',inherit-from
	   (setf (gethash 'inherit-properties hash) ',inherit-from)
	   (mapc
	    #'(lambda (other-flavor)
		(maphash
		 #'(lambda (key val)
		     (setf (gethash key hash)
			   val))
		 (flavor-wffop-hash-table 
		  (gethash other-flavor *flavor-hash-table*))))
            ',inherit-from))
         (when ',mhelp-line
	   (setf (gethash 'mhelp hash) ',mhelp-line)
	   (setf (flavor-mhelp flavor-struct) ',mhelp-line)
	   (setf (get ',flavor-name 'mhelp) 
		 (pushnew (cons 'flavor ',mhelp-line)
			  (get ',flavor-name 'mhelp))))
         (when ',attributes
	   (setf (gethash 'instance-attributes hash) ',attributes))
         (mapc #'(lambda (prop)
                   (setf (gethash (car prop) hash) (cdr prop)))
               ',hash-list)
         (setf (flavor-wffop-hash-table flavor-struct) hash)
         (setf (flavor-constructor-fun flavor-struct)
               (if ,is-structured
                   (conc-names "MAKE-" ',flavor-name)))
         (setf (gethash ',flavor-name *flavor-hash-table*) flavor-struct))
    ,@defun-list)))


(defun read-flavor-fn (wffop-name fnspec fn-name)
  (if (get wffop-name 'multiple-recursion)
      (read-n-arg-fnspec fnspec (cons 'label-arg (getargnames wffop-name))
			 fn-name)
      (read-n-arg-fnspec fnspec (getargnames wffop-name)
			 fn-name)))


(defmacro define-label (symbol flavor)
  `(let ((make-fn
	  (flavor-constructor-fun
	   (gethash ,flavor *flavor-hash-table*))))
     (if make-fn
	 (setq ,symbol (funcall make-fn))
	 (setf (get ,symbol 'flavor) ,flavor))))

;;; The following is a utility which helps in creating names for labels
;;; with a given prefix.

(defun create-name (prefix)
  (let ((counter (get prefix 'counter 0)))
    (putprop prefix (1+ counter) 'counter)
    (conc-names prefix (format nil "~A" counter))))

(defun create-namestring (prefix)
  (let ((counter (get prefix 'counter 0)))
    (putprop prefix (1+ counter) 'counter)
    (format nil "~A~A" (symbol-name prefix) counter)))

(defun create-namestring-with-hyphen (prefix)
  (let ((counter (get prefix 'counter 0)))
    (putprop prefix (1+ counter) 'counter)
    (format nil "~A-~A" (symbol-name prefix) counter)))

(defun reset-name-counter (prefix)
  (putprop prefix 0 'counter))

(defgwff-type flavor-type
  (checkfn flavor-ckfn)
  (getfn flavor-getfn)
  (mhelp "label : a label for a wff."))

(defun flavor-ckfn (xxx)
  (or (and (symbolp xxx) (get xxx 'flavor)
       (not (member (get xxx 'flavor) '(weak literal))))
      (gethash (type-of xxx) *flavor-hash-table*)))

(defun flavor-getfn (xxx)
  xxx)
