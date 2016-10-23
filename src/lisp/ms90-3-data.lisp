;;; -*- Mode:LISP; Package:auto -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1990 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;

(in-package :auto)
(part-of ms90-3)

(deffile ms90-3-data
  (part-of ms90-3)
  (mhelp "Containing the data structure used for CONNECTION
and some macros. It is a good idea to put more data structures
into the code for ms90-3 search process from now on."))

;;;subpath is a simplified path, containing only literals.
;;;We don't need universal jforms around there.
(defvar conn-debug nil)

(defstruct (conn (:print-function 
                     (lambda (x y z) 
                         (declare (ignore y z))
                         (if conn-debug
                             (progn (terpri) (princ "---------------------") (terpri)
                                  (princ "lit1 is ") (princ (conn-lit1 x)) (terpri)
                                  (princ "lit2 is ") (princ (conn-lit2 x)) (terpri)
                                  (princ "path is ") (princ (conn-path x)) (terpri) 
                                  (princ "subpath is ") (princ (conn-subpath x)) (terpri) 
                                  (princ "env is ") (princ (conn-env x)) (terpri)
                                  (princ "old-env is ") (princ (conn-old-env x)) (terpri)
                                  (princ "---------------------") (terpri))
                             (princ (cons (conn-lit1 x) (conn-lit2 x)))))))
  (lit1 nil :type literal)
  (lit2 nil :type literal)
  (old-env nil :type list)
  (env nil :type list)
  (path nil :type list)
  (subpath nil :type list))

(eval-when (compile eval load)

(defmacro car-conn (conn)
  `(conn-lit1 ,conn))

(defmacro cdr-conn (conn)
  `(conn-lit2 ,conn))
)

(defmacro path-to-subpath (path)
   `(remove-if-not #'literal-p ,path))



