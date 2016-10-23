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

;;;
;;; File: Wffsav
;;;
;;; Commands and functions for saving wffs into files.
;;;

(deffile wffsav
  (part-of save-wffs)
  (extension clisp)
  (mhelp "Commands and functions for saving wffs in files."))

(context saving-wffs)

(defvar default-lsp-file (make-pathname% :type source-extension))
; I don't think this works; it seems to fix the value at compile time. Hence the
; (let (default-lsp-file) ... ) expressions below (this variable is only used in
; this file).

(defmexpr append-wffs
  (argtypes weak-label-list filespec)
  (argnames weak-labels filename)
  (arghelp "List of Weak Labels to Write" "File to Append to")
  (defaultfns
    (lambda (weak-labels filename)
      (when (eq filename '$)
	    (let ((default-lsp-file (make-pathname% :type source-extension)))
	    (setq filename (merge-pathnames "savedwffs" default-lsp-file))))
      (list weak-labels filename)))
  (mhelp "Append the definitions of a list of weak labels to a file.
If the file does not yet exist, it will be created. You may wish to use LIB
    instead."))

(defun append-wffs (weak-labels filename)
  (let ((default-lsp-file (make-pathname% :type source-extension)))
  (reroute-output-append filename default-lsp-file
    (dolist (weak-label weak-labels)
      (write-single-wff weak-label (help-string-default weak-label))))))

(defun help-string-default (weak-label)
  (or (and (not (eq weak-label '$))
	   (cdr (assoc 'savedwff (get weak-label 'mhelp))))
      ""))

(defmexpr append-wff
  (argtypes weak-label string filespec)
  (argnames weak-label help-string filename)
  (arghelp "Weak Label to Write" "Help for Weak Label" "File to Append to")
  (defaultfns
    (lambda (weak-label help-string filename)
      (when (eq help-string '$)
	    (setq help-string (help-string-default weak-label)))
      (when (eq filename '$)
	    (let ((default-lsp-file (make-pathname% :type source-extension)))
	    (setq filename (merge-pathnames "savedwffs" default-lsp-file))))
      (list weak-label help-string filename)))
  (mhelp "Append a definition of a weak label to a file.  If the file does
not yet exist, it will be created. You may wish to use LIB instead."))

(defun append-wff (weak-label help-string filename)
  (let ((default-lsp-file (make-pathname% :type source-extension)))
  (reroute-output-append filename default-lsp-file
    (write-single-wff weak-label help-string))))

;;;
;;; The next section is to define an EDOP for quick saving of wffs into
;;; a file.
;;;

(defwffop sv-wff
  (argtypes symbol gwff)
  (argnames label gwff)
  (arghelp "Weak Label Name to Save Wff Under" "Wff to Save")
  (applicable-p (lambda (label gwff) (declare (ignore gwff))
			(not (label-q label))))
  (resulttype ignore)
  (mhelp "Save a wff by appending it to the file SAVEDWFFS. The 
weak label name should not already exist (if it does, remove it
using RW). The wffs that are saved to this file can be reloaded
using the command QLOAD \"savedwffs.lisp\".
  This command dates from before the LIBRARY top level was 
introduced; you should probably avoid it. If you want to save 
a gwff, use CW to create a weak label, then go into the library
with LIB and use INSERT to save the wff."))

(defedop save
  (alias sv-wff)
  (result-> ignore)
  (edwff-argname gwff))

(defun sv-wff (label gwff)
  (create-weak label gwff)
  (append-wff label "" "savedwffs")
  '||)


(defun write-single-wff (weak-label help-string)
  (msg t "(DEFSAVEDWFF " weak-label
       t (t 2) "(REPRESENTS ")
  (in-mode re-read (msg ((get-weak weak-label) . gwff)))
  (msg ")")
  (when (not (string= help-string ""))
	(msg t (t 2) "(MHELP ")
	(prin1 help-string) (msg ")"))
  (msg ")" t t))
