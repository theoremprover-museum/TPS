;;; -*- Mode:LISP; Package:CORE -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :CORE)
(part-of WFF-PARSE)

;;;; I don't think this file is usable at all anymore.  Removing it
;;;; from wff-parse's definition.  DAN 13MAR91

(deffile wffing
  (part-of wff-parse)
  (extension lsp)
  (mhelp "contains the rwff definition for RD, generic version of RDC."))

(context wff-parsing)

(defun rd ()
  (declare (special *lastrd*))
  (let ((bytestream (bytestream-tty)))
    (setq *lastrd* (readlist (append '(|"|) bytestream '(|"|))))
    (lexlist-parse (lexscan bytestream))))


(defgwff-type rd-type
  (checkfn rd-ckfn)
  (getfn rd-getfn)
  (mhelp "rd : Call to RD."))

(defun rd-ckfn (xxx)
  (eq xxx 'rd))

(defun rd-getfn (xxx)
  (declare (ignore xxx))
  (prog2
   (msg "Rd")
   (rd)
   (terpri)))

;;;
;;;(DC BYTESTREAM-TTY) 
;;;(BYTESREAM-TTY)   [Expr]
;;;
;;;This function receives a stream of bytes from the currently opened
;;;input device.  An escape character terminates the stream.  Carriage
;;;returns and line feeds are replaced by blanks, and lower case letters
;;;are converted to upper case.  Control Y, line continuations, are ignored
;;;properly when they are followed by a line feed and carriage control.  A
;;;list of interned characters are returned.  
;;;

(defun bytestream-tty ()
  (do ((byte (tyi) (tyi))
       (bytestream nil (cons (intern-str (string (tps-ascii byte))) bytestream)))
      ((= byte 27)
       (nreverse bytestream))
    (cond ((or (= byte 10) (= byte 13) (= byte 9))
	   (setq byte 32))
	  ((and lowercaseraise (> byte 96) (> 123 byte))
	   (setq byte (- byte 32))))))

