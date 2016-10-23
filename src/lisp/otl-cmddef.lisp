;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of OTLRULES)

;;;
;;; File: Otl-CmdDef
;;; Package: Otlrules
;;;
;;; defines functions and macros which are used inside the final rule
;;; command definitions.
;;;

(deffile otl-cmddef
  (part-of otlrules)
  (extension lsp)
  (mhelp "Defines functions and macros which are used inside the final rule
command definitions."))

;*;(defcontext rule-commands
;*;  (short-id "Rule Commands")
;*;  (order 143)
;*;  (mhelp "Commands implementing rule of inference."))

(defun meta-ize-gwff-ext (ext-gwff)
  (meta-ize-gwff (gettype 'gwff ext-gwff)))

;;; The main purpos of DEFRULEFILE is to provide a way for the automatic
;;; help to find the file with the rule definition.
;;; RULES-TABLE is a list ((file rule ... rule) (file rule ...) ...)

(defvar rules-table nil)

(context otl-object)

(defmacro defrulefile (filename &rest props)
  (let ((contents (cdr (assoc 'contents props))))
    `(progn (push ',(cons filename contents) rules-table))))

(defmacro defrulewffs (rule &rest props)
  (let ((line-wffs (cdr (assoc 'unique-lines props)))
	(restr-wffs (cdr (assoc 'unique-restrictions props)))
	(just-wffs (cdr (assoc 'unique-justifications props))))
    `(progn
      (let ((meta-var-name (prepend ',rule '-mv))
	    (meta-bdvar-name (prepend ',rule '-bd))
	    (meta-label-name (prepend ',rule '-ml))
	    (propsymalist nil))
	(declare (special meta-var-name meta-bdvar-name
			  meta-label-name propsymalist))
	(reset-name-counter meta-var-name)
	(reset-name-counter meta-bdvar-name)
	(reset-name-counter meta-label-name)
	(do ((line-wffs ',line-wffs (cdr line-wffs))
	     (parsed-line-wff))
	    ((null line-wffs))
	  (let ((meta-label-name 'ml))
	    (declare (special meta-label-name))
	    ;; We locally bind META-LABEL-NAME to make sure meta-ize-gwff
	    ;; will pick the right numbers for meta-labels during the
	    ;; meta-ize-gwff below.
	    (setq parsed-line-wff (gettype 'gwff0 (cadar line-wffs))))
	  (putprop (caar line-wffs)
		   (meta-ize-gwff parsed-line-wff)
		   'meta-assertion))
	(do ((restrs ',restr-wffs (cdr restrs)))
	    ((null restrs))
	  (putprop (caar restrs)
		   (cons (caadar restrs)
			 (mapcar #'meta-ize-gwff-ext (cdadar restrs)))
		   'restr-call)))
      (when ',just-wffs (complain f "No justifications implemented yet."))
      ',rule)))

(defmacro macro-do (do-vars &rest forms)
  (let ((local-varlist nil)
	(local-valuelist nil)
	(quoted-varlist nil)
	(quoted-valuelist nil)
	(unquoted-varlist nil)
	(unquoted-valuelist nil)
	local-value-name-list)
    (do ((do-vars do-vars (cdr do-vars)))
	((null do-vars))
      (ecase (caar do-vars)
	(local (push (cadar do-vars) local-varlist)
	       (push (caddar do-vars) local-valuelist))
	(quoted (push (cadar do-vars) quoted-varlist)
		(push (caddar do-vars) quoted-valuelist))
	(unquoted (push (cadar do-vars) unquoted-varlist)
		  (push (caddar do-vars) unquoted-valuelist))))
    (setq local-value-name-list (mapcar #'(lambda (loc-val)
					    (declare (ignore loc-val))
					    (gensym))
					local-valuelist))
    (do ((quoted-valuelist quoted-valuelist (mapcar #'cdr quoted-valuelist))
	 (unquoted-valuelist unquoted-valuelist
			     (mapcar #'cdr unquoted-valuelist))
	 (resultform
	  nil
	  (cons `(progn
		  ,@(mapcan #'(lambda (loc-var loc-val-name)
				`((setq ,loc-var (car ,loc-val-name))
				  (setq ,loc-val-name (cdr ,loc-val-name))))
			    local-varlist local-value-name-list)
		  ,@(sublis
		     (append (mapcar #'(lambda (q-var q-val-list)
					 (cons q-var `',(car q-val-list)))
				     quoted-varlist quoted-valuelist)
			     (mapcar #'(lambda (uq-var uq-val-list)
					 (cons uq-var (car uq-val-list)))
				     unquoted-varlist unquoted-valuelist))
		     forms))
		resultform)))
	((or (and quoted-valuelist (member 'nil quoted-valuelist))
	     (and unquoted-valuelist (member 'nil unquoted-valuelist)))
	 `(let ,(append
		 (mapcar #'(lambda (loc-val-name loc-val)
			     (list loc-val-name loc-val))
			 local-value-name-list local-valuelist)
		 local-varlist)
	    ,@(nreverse resultform))))))

(defmacro setq-resolve-conflict (var value)
  `(if (eq ,var '$) (setq ,var ,value)
       (if (not (eq ,value '$)) (setq ,var (min ,var ,value)))))

(defmacro setq-destruct (struct struct-values)
  `(let ((struct-values ,struct-values))
     (setq-destruct-rec ,struct struct-values nil)))

(defmacro setq-destruct-rec (struct struct-values multi-vars)
  (cond ((null struct) `nil)
	((symbolp struct)
	 (if (member struct multi-vars)
	     `(setq-resolve-conflict ,struct ,struct-values)
	     `(setq ,struct ,struct-values)))
	((consp struct)
	 (if (cdr struct)
	     (if (eq (car struct) 'quote)
		 `nil
		 `(progn
		   (setq-destruct-rec ,(car struct) (car ,struct-values)
				      ,multi-vars)
		   (setq-destruct-rec ,(cdr struct) (cdr ,struct-values)
				      ,multi-vars)))
	     `(setq-destruct-rec ,(car struct) (car ,struct-values)
				 ,multi-vars)))
	(t (throwfail))))

(defmacro setq-destruct-multi (multi-vars struct struct-values)
  `(let ((struct-values ,struct-values))
     (setq-destruct-rec ,struct struct-values ,multi-vars)))

(defmacro eval-destruct (struct)
  (cond ((null struct) nil)
	((symbolp struct) (throwfail))
	((consp struct)
	 (if (or (symbolp (car struct)) (eq (caar struct) 'quote))
	     `(list ,@struct)
	     `(cons (eval-destruct ,(car struct))
		    (eval-destruct ,(cdr struct)))))
	(t (throwfail))))

(defflag auto-generate-hyps
  (flagtype boolean)
  (default t)
  (subjects outline)
  (mhelp "If T, hypotheses for lines computed and filled in automatically,
if NIL, the user will be asked for confirmation for each set of hypotheses."))

