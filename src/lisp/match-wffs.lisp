;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFFMATCH)

;;;
;;; File: Match-Wffs
;;; Package: Wffmatch
;;;
;;; defines the MATCH-BIND and SUBSTITUTE-BINDINGS functions used
;;; by the rules package and the outline commands produced by it.
;;;

(part-of wffmatch)

(deffile match-wffs
  (part-of wffmatch)
  (extension lsp)
  (mhelp "Defines the MATCH-BIND and SUBSTITUTE-BINDINGS functions used
by the rules package and the outline commands produced by it."))


(defun getsubstpair (pair)
  (let ((gvar (getwff-subtype 'gvar-p (car pair))))
    (let ((global-type (type gvar)))
      (declare (special global-type))
      (cons gvar (getwff-subtype 'gwff-p (cadr pair))))))

(defun princsubstpair (pair)
  (let ((displaywff nil))
    (declare (special displaywff))
    (msg t ((car pair) . gwff) " <-- " ((cdr pair) . gwff))))



(defwffop match
  (argtypes gwff gwff)
  (argnames wffschema gwff)
  (resulttype gwffalist)
  (arghelp "The wff schema with meta-variables" "The matched wff")
  (applicable-p (lambda (wffschema gwff) (type-equal wffschema gwff)))
  (mhelp "Test whether a wff matches a wff schema."))



(defun match (wffschema gwff)
  (let ((wffbindings nil))
    (declare (special wffbindings))
    (match-bind wffschema gwff)
    wffbindings))

(defun match-bind (wffschema gwff)
  (declare (special wffbindings))
  (cond ((label-q wffschema)
	 (apply-label wffschema (match-bind wffschema gwff)))
	((label-q gwff)
	 (apply-label gwff (match-bind wffschema gwff)))
	((lsymbol-q wffschema)
	 (when (or (not (lsymbol-q gwff))
		   (not (same-const-p wffschema gwff)))
	       (mismatch% wffschema gwff)))
	((lsymbol-q gwff)
	 (mismatch% wffschema gwff))
	((boundwff-q wffschema)
	 (cond ((not (boundwff-q gwff))
		(mismatch% wffschema gwff))
	       ((not (equal (cdar wffschema) (cadr gwff)))
		(mismatch% wffschema gwff))
	       (t (match-bind (caar wffschema) (caar gwff))
		  (match-bind (cdr wffschema) (cdr gwff)))))
	(t (match-bind (car wffschema) (car gwff))
	   (match-bind (cdr wffschema) (cdr gwff)))))

(defun meta-var-match-bind (label gwff)
  (declare (special wffbindings default-wffeq))
  (let ((wffvalue (wffeval label)))
    (if wffvalue
	(when (not (same-match-p label wffvalue gwff))
	      (throwfail "Illegal Match.  " (label . gwff)
			 " matched " (wffvalue . gwff) " somewhere and now "
			 (gwff . gwff) "," t "but they are not the same!"))
	(wffset label gwff))))

(defun meta-bdvar-match-bind (label gwff)
  (declare (special wffbindings))
  (let ((bdwff (get label 'meta-wff)))
    (cond ((label-q gwff) (apply-label gwff (match-bind label gwff)))
	  ((lsymbol-q gwff) (mismatch% bdwff gwff))
	  ((not (boundwff-q gwff)) (mismatch% bdwff gwff))
	  (t (when (not (eq (cdar bdwff) (cdar gwff)))
		   (mismatch% bdwff gwff))
	     (wffset (caar bdwff) (caar gwff))
	     (match-bind (cdr bdwff) (cdr gwff))
	     ;;Here we could perhaps undo the binding if we did allow
	     ;;wffschemas which are not alphabetically normal.
	     ))))

(defun meta-label-match-bind (label gwff)
  (declare (special wffbindings default-wffeq))
  (let ((wffvalue (wffeval label)))
    (if wffvalue
	(when (not (same-match-p label wffvalue gwff))
	      (throwfail "Illegal Match.  " (label . gwff)
			 " matched " (wffvalue . gwff) " somewhere and now "
			 (gwff . gwff) "," t "but they are not the same!"))
	(wffset label gwff))))

(defun same-const-p (const1 const2)
  (cond ((logconst-q const1)
	 ;; cannot be propsym, otherwise case would be here
	 (eq const1 const2))
	((or (pmpropsym-q const1) (pmabbrev-q const1))
	 (eq (get const1 'stands-for) (get const2 'stands-for)))
	((abbrev-q const1)
	 (eq const1 const2))
	(t (throwfail "Illegal Proper Symbol " (const1 . gwff)
		      " in wffschema."))))

;;; Everything coming now is the substitution of bound meta-variables,
;;; rather the matching process.


(defun msubst (substs gwff)
  (let ((propsymalist nil)
	(wffconstlist nil))
    (declare (special propsymalist wffconstlist))
    (setq gwff (make-wffschema1 gwff))
    ;;(msg t (gwff . gwff))
    ;;(funcall prin1 propsymalist)
    (let ((wffbindings
	   (mapcar #'(lambda (subst-pair)
		       (declare (special propsymalist))
		       (let ((apair (assoc (car subst-pair) propsymalist)))
			 (if apair (cons (cdr apair) (cdr subst-pair))
			     subst-pair)))
		   substs)))
      (declare (special wffbindings))
      (meta-subst gwff))))

(defwffrec meta-subst (argnames wffschema))

(defun meta-subst (wffschema)
  (let ((pm-flag nil)
	(typeassoc nil)
	(global-type nil))
    (declare (special pm-flag global-type typeassoc))
    (let ((subst-wff (meta-subst1 wffschema)))
      ;; If we had a polymorphic symbol, we must infer its type
      (when pm-flag (setq subst-wff (finalscan subst-wff)))
      subst-wff)))

(defwffrec meta-subst1 (argnames wffschema))

(defun meta-subst1 (wffschema)
  (declare (special wffbindings pm-flag))
  (cond ((label-q wffschema)
	 (apply-label wffschema (meta-subst1 wffschema)))
	((lsymbol-q wffschema)
	 (cond ((or (logconst-q wffschema) (propsym-q wffschema))
		wffschema)
	       ((or (pmpropsym-q wffschema) (pmabbrev-q wffschema))
		(let ((new-genlsym (gensym)))
		  (putprop new-genlsym (get wffschema 'stands-for) 'stands-for)
		  (setq pm-flag t)
		  new-genlsym))
	       (t wffschema)))
	((boundwff-q wffschema)
	 (cons (car wffschema) (meta-subst1 (cdr wffschema))))
	(t (cons (meta-subst1 (car wffschema))
		 (meta-subst1 (cdr wffschema))))))

(defun meta-subst-meta-var (label)
  (declare (special wffbindings))
  (let ((wffvalue (wffeval label)))
    (if (not wffvalue)
    	;;If some meta variable is undefined, we do a THROWFAIL for now.
    	;;For more see below.
    	(throwfail "Meta variable " label " does not have a value.")
    	wffvalue)))

(defun meta-subst-meta-bd (label)
  (declare (special wffbindings meta-bdvar-name))
  (let ((bdwff (get label 'meta-wff)))
    (let ((wffvalue (wffeval (caar bdwff))))
      ; mkaminski 11/11/2005 -- meta-wffs created using procedures in meta-var2
      ;   may contain proper symbols.
      (if (propsym-p (bdvar bdwff))
	  (acons (bdvar bdwff) (cdar bdwff) (meta-subst1 (cdr bdwff)))
	(if (not wffvalue)
	  ;;For now we do a THROWFAIL in this case.  This is neccessary
	  ;;So that META-LABELS are handled correctly.  The old code is
	  ;;below
	    (throwfail "Meta variable " (caar bdwff) " did not have a value.")
      	  ;;(let ((new-meta-bd-label (create-name meta-bdvar-name))
      		;;(new-bd-meta (cons (car bdwff) (meta-subst1 (cdr bdwff)))))
      	  ;;(putprop new-meta-bd-label 'meta-bd 'flavor)
      	  ;;(putprop new-meta-bd-label new-bd-meta 'meta-wff)
      	  ;;new-meta-bd-label)
      	  (cons (cons wffvalue (cdar bdwff)) (meta-subst1 (cdr bdwff))))))))
	

(defun meta-label-subst (label)
  (declare (special wffbindings))
  ;; This function will lead to a throwfail if not all meta-labels are defined
  (let ((wffvalue (wffeval label)))
    (if wffvalue wffvalue
	(progn
	 (wffset label
		 (apply (get label 'meta-wffop)
			(mapcar #'meta-subst-if-wff (get label 'meta-args))))
	 (wffeval label)))))

(defun meta-subst-if-wff (wffop-arg)
  (declare (special wffbindings))
  (if (and (symbolp wffop-arg) (get wffop-arg 'lisp-token))
      wffop-arg
      (meta-subst wffop-arg)))

;;; META-LABEL-EVAL differs from meta-label-subst in that it does not
;;; look for a binding, but always(!) evaluates the meta-label by
;;; applying the META-WFFOP.  Also, it does not enter the result into the
;;; binding list.

(defun meta-label-eval (label)
  (declare (special wffbindings))
  ;; This function will lead to a throwfail if not all meta-labels are defined
  (apply (get label 'meta-wffop)
	 (mapcar #'meta-subst-if-wff (get label 'meta-args))))

