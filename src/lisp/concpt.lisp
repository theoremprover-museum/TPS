;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of CONCEPT-BARE)

;;;
;;; File: Concpt
;;;
;;; contains function useful for the CONCEPT terminal, if windows or the
;;; Pad are not used!
;;;

(deffile concpt
  (part-of concept-bare)
  (extension clisp)
  (mhelp "Contains functions for Concept terminal if neither the windows
nor the Pad are used."))

(context concept-terminal)

(defvar kset nil)

(defun tyol (&rest integers)
  (dont-count
   (tyo 27)
   (dolist (char integers) (tyo char))))
  
;;This is no longer used anywhere.

#+comment(defun tyoi (&rest integers)
  (do ((chars integers (cdr chars)))
      ((null chars))
    (tyo (car chars) image-file-object)))

(defun tyos (n) (tyol 114 n 33))

(defun load-kset-keys ()
  (tyol 52 34 63 34 27 48)		;<esc> 0 to f11
  (tyol 52 34 64 34 27 49)		;<esc> 1 to f12
  (tyol 52 34 65 34 27 50)		;<esc> 2 to f13
  (tyol 52 34 66 34 27 51)		;<esc> 3 to f14
  (tyol 52 34 67 34 27 53)		;<esc> 5 to F12
  (tyol 52 34 68 34 27 54)		;<esc> 6 to F13
  (tyol 52 34 69 34 27 55)		;<esc> 7 to F14
  (tyol 52 34 47 34 27 52))		;<esc> 4 to F11

(defmexpr loadkey
  (argtypes integer+ anything)
  (argnames key mssg)
  (arghelp "Key, e.g. 5 for f5" "Symbol or string to load into function key")
  (mhelp "Load one of the function keys f1-f10 on a concept terminal with a string."))

(defun loadkey (key mssg)
  (dont-count
   (tyol 52 (+ (flatc mssg) 32) (+ 52 key) 34) ;; changed flatsizec to flatc
   (princ mssg)))                              ;; 9/18/87 DAN

(defun enter-kset (ks)
  (declare (special kset))
  (when (not (= ks kset))
    (tyol 106 (+ 32 ks))
    (setq kset ks)))


(defmexpr reset
  (mhelp "Put a Concept terminal into correct mode and load the function keys."))

(defun reset ()
  (terminal-image-mode)
  (shortreset) nil)

(defun shortreset ()
  (declare (special kset))
  (tyol 85)				; prog mode
  (tyol 106 32)				; enter kset 0
  (setq kset 0)
  (load-kset-keys)			; load character-set reading keys.
  )

(defvar transparent-mode nil)

(defun transparent-mode-off ()
  (when transparent-mode (tyol 116) (setq transparent-mode nil)))

(defun transparent-mode-on ()
  (when (not transparent-mode) (tyol 84) (setq transparent-mode t)))


