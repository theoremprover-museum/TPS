;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

;;;
;;; File TEST-TOP-LIB
;;;
;;; defines library functions to do with searchlists.


(in-package :core)

(context library)

(deffile test-top-lib
  (part-of library) 
  (extension lisp)
  (mhelp "Defines functions to do with searchlists and modes for test-top."))

(defun test-fetch (name type) (retrieve-libobject-real name type))

(defun test-delete (name type) (delete-libobject name type))

(defun test-insert (name type comment)
  (if (or (and (eq type 'slist) (auto::known-searchlist-p name))
	  (and (or (eq type 'mode) (eq type 'core::mode1))
	       (member name global-modelist)))
      (insert-libobject name type comment)
    (msg "No such object is known; can only save known modes and searchlists from" t "the TEST top level." t)))

;;SLIST

(deflibobject slist
  (lib-promptfn lib-promptfn-slist)
  (lib-printfn libprint-slist)
  (lib-tpsobject get-lib-slist)
  (mhelp "The library object corresponding to a searchlist."))

(defun libprint-slist (slist libstyle)
  (declare (ignore libstyle))
  (msg "(")
  (dolist (sl slist (msg ")"))
	  (msgf sl)))
;  (when (memq style '(tex tex-1)) (princ "$"))
;  (princ slist))

(defun lib-promptfn-slist (name type help file modify comment old-value)
  (declare (ignore type help file modify comment old-value))
  (values (auto::slist-to-list name) nil 
	  (auto::searchlist-function (auto::find-searchlist name auto::*global-searchlist*))))

(defun get-lib-slist (libitem)
  (auto::list-to-slist (libitem-name libitem) (libitem-description libitem) (libitem-other-attributes libitem)))


