;;; -*- Mode:LISP; Package:TEACHER -*-
;;; ******************************************************************* ;;;
;;;         (c) Copyrighted 1988 by Carnegie Mellon University.         ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the TPS project.           ;;;
;;;   If you want to use this code or any part of TPS, please contact   ;;;
;;;               Peter B. Andrews (Andrews@CS.CMU.EDU)                 ;;;
;;; ******************************************************************* ;;;
;;; ;;; File Grades ;;;

(in-package :teacher)
(part-of grader-top)

;(proclaim '(special etps-file score-file read-grade-ini patch-file))

(deffile grades-top
  (part-of grader-top)
  (extension lisp)
  (mhelp "Creates the grading package top-level."))

(context subtoplevels)

(defmexpr do-grades
  (mhelp "Invoke the grading package."))

(defun do-grades ()
  (declare (special etps-file score-file read-grade-ini patch-file))
  (load-module 'grader)
  (setq etps-file (if (boundp 'score-file) score-file ""))
  (unless read-grade-ini
    (let ((patch-file (locate-tps-file patch-file t)))
      (if patch-file (load patch-file)))
    (let ((init-file (make-pathname% :name "grader" :type "ini")))
      (when                             ;(probe-file init-file)
          ;; changed so that homedir and source-path will be
          ;; checked for init file if not one in current dir -- DAN
          (setq init-file (locate-tps-file init-file t))
        (load init-file)
        (msg "Init File " (init-file . filespec) " read.")	
        (setq read-grade-ini T))))
  (initialize-filenames)
  (initialize1)
  (%catch% (gradetop)
	   (exit-inferior-top core::expand-catch-throw))
  )
