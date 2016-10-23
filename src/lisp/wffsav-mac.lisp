;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of SAVE-WFFS)

(deffile wffsav-mac
  (part-of save-wffs)
  (extension lsp)
  (mhelp "Categories, argument types etc. for saving wffs in files."))

(context saving-wffs)

(defun weak-label-p (xxx)
  (and (symbolp xxx) (eq (get xxx 'flavor) 'weak)))

(defcategory savedwff
  (define defsavedwff)
  (properties
   (represents read-weak-label)
   (mhelp single))
  (global-list global-savedwfflist)
  (mhelp-line "saved wff")
  (mhelp-fn savedwff-mhelp))

(defun savedwff-mhelp (saved-wff category)
  (msgf "Defined as " ((get-weak saved-wff) . gwff))
  (princ-mhelp saved-wff category))

(defun read-weak-label (gwff-string weak-label-name)
  (in-mode re-read
   (let ((first-order-mode-parse nil))
     (prog1
	 (gettype 'gwff (car gwff-string))
       (putprop weak-label-name 'weak 'flavor)))))
