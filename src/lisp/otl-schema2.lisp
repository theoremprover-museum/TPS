;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of OTLSCHEMA2)

;;;
;;; File: OTL-SCHEMA2
;;; Package: OTLSCHEMA2
;;;
;;; defines a way of using theorem schemas without restrictions
;;; as lemmas.

;;; IMPORTANT: This duplicates material in OTL-SCHEMA, except that
;;; this defines command ASSERT instead of TI and allows type inference.

(part-of otlschema2)

(deffile otl-schema2
  (part-of otlschema2)
  (extension lsp)
  (mhelp "Defines a way of using theorem schemas without restrictions as lemmas."))

(defun assert-legal (theorem line)
  (declare (ignore line))
  (when (and (theorem-p dproof)
	     ;;If DPROOF is not a theorem, we are doing our own exercise
	     ;; (started with PROVE) in which case we allow anything.
	     (or (null (get dproof 'allowed-lemma-p))
		 (not (funcall (get dproof 'allowed-lemma-p)
			       dproof theorem))))
    (throwfail "It is illegal to use theorem " (theorem . theorem)
	       " in the proof of exercise " (dproof . theorem) ".")))

;;; The following will not work in MacLisp.  Use a macro instead.

;*;(defun const-fun (value)
;*;  #'(lambda (ignore) (declare (ignore ignore)) value))

;;; Silly.  DAN 10MAR89
;(defmacro const-fun (value)
;  `#'(lambda (ignore) (declare (ignore ignore)) ,value))

;;; Changed to give more help, defaults.  Changed name to ASSERT% to
;;; avoid conflict with the standard macro ASSERT. DAN 10MAR89

(defun make-assert-argtype-list (psymlist mwff)
  (let ((find-list (mapcar #'(lambda (x) (cons (cdr x) T)) psymlist))) 
    ;;should look like ((mv0 . T) (mv1 . T)...)
    (do ((wfflist (list (or (and (symbolp mwff) (get mwff 'meta-wff))
			    mwff)))
	 (this-wff nil nil))
	((null wfflist))
      (setq this-wff (pop wfflist))
      (when (and (symbolp this-wff) (get this-wff 'meta-wff))
	(setq this-wff (get this-wff 'meta-wff)))
      (cond ((symbolp this-wff) 
	     (when (and (assoc this-wff find-list)
			(eq 't (cdr (assoc this-wff find-list))))
	       (rplacd (assoc this-wff find-list) 'gwff)))
	    ((and (consp this-wff) (consp (car this-wff)) (binder-p (cdar this-wff)))
	     (when (and (assoc (caar this-wff) find-list)
			(eq 't (cdr (assoc (caar this-wff) find-list))))
	       (rplacd (assoc (caar this-wff) find-list) 'gvar)
	       (push (cdr this-wff) wfflist)))
	    (t (push (car this-wff) wfflist)
	       (push (cdr this-wff) wfflist))))
    (mapcar #'cdr find-list)))

; mkaminski 11/10/2005 -- introduced assert%-generic

(defun assert% (theorem line)
  (assert%-generic theorem line (format nil "Theorem ~A" theorem)
		   (list (conc-strings "Assert " (string theorem))
			 nil nil)))

(defun assert%-generic (theorem line thm-name-string justification)
  (let ((meta-theorem (get-meta-assertion theorem))
	(new-label (gensym "LINE"))
	(line-label (numalias line)))
    (setf (line-linenumber new-label) line)
    (setf (line-justification new-label) justification)
    (setf (line-hypotheses new-label) 
	  (if line-label (line-hypotheses line-label)))
    (if line-label
	(progn
	  (%catch% (match meta-theorem (line-assertion line-label))
		   (fail (throwfail-on
			   "Line is not an instance of the cited theorem."
			   t expand-catch-throw)))
	  (setf (line-assertion new-label)
		(line-assertion line-label)))
      (let* ((propsymalist (get-meta-alist theorem))
	     (wfflist (mapcar #'car propsymalist))
	     (arglist (make-list (length wfflist) :initial-element '*))
	     ;(argtypes (make-list (length wfflist) :initial-element 'gwff))
	     ;some of these arguments may be quantified over; can't substitute wffs for those...
	     (argtypes (make-assert-argtype-list propsymalist (get theorem 'meta-assertion)))
	     (wffargtypes (mapcar #'type wfflist))
	     (typelist (find-type-vars wffargtypes))
	     (defaultfns (list wfflist))
	     (strong-defaultlist (make-list (length wfflist)))
	     (argnames (mapcar #'getnameroot wfflist))
	     (arghelp nil)
	     (printtypes t)
	     internal-arglist)
	(declare (special propsymalist))
	 ;(format t "~&argtypes: ~A~%" argtypes)
	 ;(msgf "Theorem " theorem " contains the following metavariables: "
	(msgf thm-name-string " contains the following metavariables: "
	      (wfflist . gwfflist) ". " t)
	(when typelist 
	  (msg "The metavariables contain these type variables: "
	       (typelist . typesymlist) "." t))
	(when (or (not expertflag) 
		  (query "Do you want extended help?" nil))
	  (msg t
	    "You will be prompted for the wffs which should be substituted for the
metavariables one at a time.  If the given metavariable is free, then you may enter 
any wff, as long as its type is consistent with the type of the metavariable, and 
the substitution of types for type variables is consistent among the wffs. 
If the metavariable is bound, you may enter any variable name, under the same 
type constraints." t t
"Example: Theorem X5203 contains the metavariables \"f(AB)\", \"x(OB)\", and
\"y(OB)\", with type variables \"A\" and \"B\".  An example of acceptable
inputs is
for f: \"lambda w(OI). z(I)\"
for x: \"lambda w(OI). truth\"
for y: \"lambda w(OI). falsehood\"." t t))
	(setq internal-arglist
	      (prompt-values 'assert arglist argtypes wffargtypes
			     typelist
			     defaultfns strong-defaultlist argnames
			     arghelp))
	(let ((wffbindings
	       (mapcar #'cons (mapcar #'cdr propsymalist) internal-arglist)))
	  (declare (special wffbindings))
	  (setf (line-assertion new-label) 
		(meta-subst meta-theorem)))))
    (update-plan (list (list new-label 'ss)) nil)))

(defun assert2 (theorem line)
  (assert2-generic theorem line (format nil "Theorem ~A" theorem)
		   (list (conc-strings "Assert " (string theorem))
			 nil nil)))

(defun assert2-generic (theorem line thm-name-string justification)
  (let ((meta-theorem
	 ;(remove-if #'(lambda (x) (not (free-in (car x)
		;				(get theorem 'assertion))))
		    (get-m-meta-assertion theorem));)
	(new-label (gensym "LINE"))
	(line-label (numalias line)))
    (setf (line-linenumber new-label) line)
    (setf (line-justification new-label) justification)
    (setf (line-hypotheses new-label) 
	  (if line-label (line-hypotheses line-label)))
    (if line-label
	(progn
	  (%catch% (match meta-theorem (line-assertion line-label))
		   (fail (throwfail-on
			   "Line is not an instance of the cited theorem."
			   t expand-catch-throw)))
	  (setf (line-assertion new-label)
		(line-assertion line-label)))
      (progn
	(putprop theorem nil 'meta-assertion)
	(let* ((meta-theorem (get-l-meta-assertion theorem))
	       (propsymalist (get-meta-alist theorem))
	       (wfflist (mapcar #'car propsymalist))
	       (arglist (make-list (length wfflist) :initial-element '*))
	       ;(argtypes (make-list (length wfflist) :initial-element 'gwff))
	       ;some of these arguments may be quantified over; can't substitute wffs for those...
	       (argtypes (make-assert-argtype-list propsymalist (get theorem 'meta-assertion)))
	       (wffargtypes (mapcar #'type wfflist))
	       (typelist (find-type-vars wffargtypes))
	       (defaultfns (list wfflist))
	       (strong-defaultlist (make-list (length wfflist)))
	       (argnames (mapcar #'getnameroot wfflist))
	       (arghelp nil)
	       (printtypes t)
	       internal-arglist)
	  (declare (special propsymalist))
	  ;(format t "~&argtypes: ~A~%" argtypes)
	  ;(msgf "Theorem " theorem " contains the following metavariables: "
	  (msgf thm-name-string " contains the following metavariables: "
		(wfflist . gwfflist) ". " t)
	  (when typelist 
	    (msg "The metavariables contain these type variables: "
		 (typelist . typesymlist) "." t))
	  (when (or (not expertflag) 
		    (query "Do you want extended help?" nil))
	    (msg t
	    "You will be prompted for the wffs which should be substituted for the
metavariables one at a time.  If the given metavariable is free, then you may enter 
any wff, as long as its type is consistent with the type of the metavariable, and 
the substitution of types for type variables is consistent among the wffs. 
If the metavariable is bound, you may enter any variable name, under the same 
type constraints." t t
"Example: Theorem X5203 contains the metavariables \"f(AB)\", \"x(OB)\", and
\"y(OB)\", with type variables \"A\" and \"B\".  An example of acceptable
inputs is
for f: \"lambda w(OI). z(I)\"
for x: \"lambda w(OI). truth\"
for y: \"lambda w(OI). falsehood\"." t t))
	  (setq internal-arglist
		(prompt-values 'assert arglist argtypes wffargtypes
			       typelist
			       defaultfns strong-defaultlist argnames
			       arghelp))
	  (let* ((wffbindings
		  (mapcar #'cons (mapcar #'cdr propsymalist) internal-arglist))
		 (typesubs (remove-duplicates
			    (apply #'append
				   (mapcar #'(lambda (i p)
					       (suggest-types i p typelist))
					   internal-arglist
					   (mapcar #'car propsymalist)))
			    :test #'equal))
		 (theorem (progn
			    (setf (get theorem 'assertion)
				  (substitute-types typesubs
						    (get theorem 'assertion)))
			    theorem))
		 (meta-theorem (get-l-meta-assertion theorem))
		 (propsymalist (get-meta-alist theorem))
		 (wffbindings
		  (mapcar #'cons (mapcar #'cdr propsymalist)
			  internal-arglist)))
	    (declare (special wffbindings))
	    (setf (line-assertion new-label) 
		  (meta-subst meta-theorem))))))
    (update-plan (list (list new-label 'ss)) nil)))

(defun get-meta-assertion (theorem)
  (cond ;((get theorem 'meta-assertion))
        ; mkaminski - we have different sorts of meta-assertions, so just
        ;   returning an existing one might not give us what we want
	(t (let ((meta-assertion nil) ;;; added nil as initial value 8/6/87 DAN
		 (propsymalist nil)
		 (wffconstlist nil))
	     (declare (special propsymalist wffconstlist))
	     (setq meta-assertion
		   (make-wffschema1
		    (funcall (get 'theorem-type 'getfn) theorem)))
	     (putprop theorem meta-assertion 'meta-assertion)
	     (putprop theorem (reverse propsymalist) 'meta-alist)
	     meta-assertion))))

; mkaminski 11/11/2005

(defun get-l-meta-assertion (theorem)
  (cond ;((get theorem 'meta-assertion)) ; see comment above
	(t (let ((meta-assertion nil)
		 (propsymalist nil)
		 (wffconstlist nil))
	     (declare (special propsymalist wffconstlist))
	     (setq meta-assertion
		   (make-l-wffschema1  ; mkaminski 11/11/2005
				       ; -- changed from make-wffschema1
		    (funcall (get 'theorem-type 'getfn) theorem)
		    nil))
	     (putprop theorem meta-assertion 'meta-assertion)
	     (putprop theorem (reverse propsymalist) 'meta-alist)
	     meta-assertion))))

(defun get-m-meta-assertion (theorem)
  (cond ;((get theorem 'meta-assertion)) ; see comment above
	(t (let ((meta-assertion nil)
		 (propsymalist nil)
		 (wffconstlist nil))
	     (declare (special propsymalist wffconstlist))
	     (setq meta-assertion
		   (make-m-wffschema1  ; mkaminski 11/11/2005
				       ; -- changed from make-wffschema1
		    (funcall (get 'theorem-type 'getfn) theorem)))
	     (putprop theorem meta-assertion 'meta-assertion)
	     (putprop theorem (reverse propsymalist) 'meta-alist)
	     meta-assertion))))

(defun get-meta-alist (theorem)
  (cond ((get theorem 'meta-alist))
	(t (get-meta-assertion theorem)
	   (get theorem 'meta-alist))))
